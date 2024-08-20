#include <algorithm>
#include <cstddef>
#include <iostream>
#include <vector>
#include <cmath>
#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <numeric>
#include "voxelgrid.h"
#ifdef MYREAL8
typedef double my_real;
#else
typedef float my_real;
#endif

constexpr int GROUP_SIZE = 128;
constexpr my_real fourth = 0.25;
constexpr my_real one = 1.0;
constexpr my_real zero = 0.0;
constexpr my_real em30 = 1e-30;
constexpr my_real em20 = 1e-20;

template <typename T>
void print_address(T &var, std::string name)
{
    std::cout << name << reinterpret_cast<std::uintptr_t>(&var) << " (decimal), " << static_cast<void *>(&var) << " (hexadecimal)" << std::endl;
}
template <typename T>
void print_address(T *var, std::string name)
{
    std::cout <<  name << "=" << reinterpret_cast<std::uintptr_t>(var) << std::endl; 
}

// Function to calculate median
double calculate_median(std::vector<double> data) {
    std::sort(data.begin(), data.end());
    if (data.size() % 2 == 0) {
        return (data[data.size() / 2 - 1] + data[data.size() / 2]) / 2.0;
    } else {
        return data[data.size() / 2];
    }
}

void adjustDimensions(size_t& nbx, size_t& nby, size_t& nbz, size_t numInts) {
    size_t currentProduct = nbx * nby * nbz;
    if (currentProduct <= numInts) {
        return;
    }
    constexpr size_t four = static_cast<size_t>(4);
    double scaleFactor = std::cbrt(static_cast<double>(numInts) / currentProduct);
    nbx = std::max(four, static_cast<size_t>(std::round(nbx * scaleFactor / 4.0)) * four );
    nby = std::max(four, static_cast<size_t>(std::round(nby * scaleFactor / 4.0)) * four );
    nbz = std::max(four, static_cast<size_t>(std::round(nbz * scaleFactor / 4.0)) * four );
    currentProduct = nbx * nby * nbz;
    while (currentProduct > numInts) {
        scaleFactor = std::cbrt(static_cast<double>(numInts) / currentProduct);
	if(nbx > four ) nbx -= four;
        if(nby > four ) nby -= four;
        if(nbz > four ) nbz -= four;
        currentProduct = nbx * nby * nbz;
    }
}


    inline void compute_coordinates(int &nbx, const my_real xmaxb, const my_real xminb, size_t &ix1, const my_real xmine, my_real aaa, size_t &ix2, const my_real xmaxe, int &nby, const my_real ymaxb, const my_real yminb, size_t &iy1, const my_real ymine, size_t &iy2, const my_real ymaxe, int &nbz, const my_real zmaxb, const my_real zminb, size_t &iz1, const my_real zmine, size_t &iz2, const my_real zmaxe)
    {
        int lx1,lx2,ly1,ly2,lz1,lz2;
        if (nbx > 1)
        {
            const my_real inv_xrange = 1.0 / (xmaxb - xminb);
            lx1 = int(nbx * (xmine - aaa - xminb) * inv_xrange);
            lx2 = int(nbx * (xmaxe + aaa - xminb) * inv_xrange);
        }
        else
        {
            lx1 = -2;
            lx2 = 1;
        }

        if (nby > 1)
        {
            const my_real inv_yrange = 1.0 / (ymaxb - yminb);
            ly1 = int(nby * (ymine - aaa - yminb) * inv_yrange);
            ly2 = int(nby * (ymaxe + aaa - yminb) * inv_yrange);
        }
        else
        {
            ly1 = -2;
            ly2 = 1;
        }

        if (nbz > 1)
        {
            const my_real inv_zrange = 1.0 / (zmaxb - zminb);
            lz1 = int(nbz * (zmine - aaa - zminb) * inv_zrange);
            lz2 = int(nbz * (zmaxe + aaa - zminb) * inv_zrange);
        }
        else
        {
            lz1 = -2;
            lz2 = 1;
        }
        ix1 = static_cast<size_t>(std::max(1, 2 + std::min(nbx, lx1)));
        iy1 = static_cast<size_t>(std::max(1, 2 + std::min(nby, ly1)));
        iz1 = static_cast<size_t>(std::max(1, 2 + std::min(nbz, lz1)));
        ix2 = static_cast<size_t>(std::max(1, 2 + std::min(nbx, lx2)));
        iy2 = static_cast<size_t>(std::max(1, 2 + std::min(nby, ly2)));
        iz2 = static_cast<size_t>(std::max(1, 2 + std::min(nbz, lz2)));
        ix1--;
        iy1--;
        iz1--;
        ix2--;
        iy2--;
        iz2--;

    }

// scale voxel dimensions to fit within the allocated size in Fortran code
void scale_dimensions(int &nbx, int &nby, int &nbz, long long cell_max) {
    const long long max_volume = std::min(static_cast<long long>(8000000), cell_max);

    long long current_volume = static_cast<long long>(nbx+2) * (nby+2) * (nbz+2);

    if (current_volume > max_volume) {
        // Calculate the scaling factor
        double scale_factor = std::cbrt(static_cast<double>(max_volume) / current_volume);

        // Scale each dimension using the scale factor
        nbx = std::max(1, static_cast<int>(nbx * scale_factor));
        nby = std::max(1, static_cast<int>(nby * scale_factor));
        nbz = std::max(1, static_cast<int>(nbz * scale_factor));

        // Recalculate the current volume after scaling
        current_volume = static_cast<long long>(nbx+2) * (nby+2) * (nbz+2);

        // Adjust dimensions if the product is still greater than max_volume
        while (current_volume > max_volume) {
            if (nbx >= nby && nbx >= nbz && nbx > 1) {
                nbx--;
            } else if (nby >= nbx && nby >= nbz && nby > 1) {
                nby--;
            } else if (nbz >= nbx && nbz >= nby && nbz > 1) {
                nbz--;
            }
            current_volume = static_cast<long long>(nbx+2) * (nby+2) * (nbz+2);
        }
    }
}


extern "C"
{
    void fortran_integer_realloc(int* a, int *oldsize, int * newsize);
    void fortran_real_realloc(my_real* a, int *oldsize, int* newsize);


    void free_c_int(int *& a)
    {
        std::free(a);
        a = nullptr;
    }
    void cpp_inter7_gather_cand(int jlt, const my_real *x, const int *irect, const int *nsv, int *cand_e,
                                int *cand_n, int igap, my_real gap, my_real *gapv,
                                const my_real *gap_s, const my_real *gap_m, const my_real *curv_max,
                                my_real gapmax, my_real gapmin, my_real dgapload,
                                my_real drad, my_real *x1, my_real *x2, my_real *x3,
                                my_real *x4, my_real *y1, my_real *y2, my_real *y3,
                                my_real *y4, my_real *z1, my_real *z2, my_real *z3,
                                my_real *z4, my_real *xi, my_real *yi, my_real *zi,
                                const my_real *gap_s_l, const my_real *gap_m_l, int s_xrem, int nsn,
                                int nsnr, const my_real *xrem, int *ix1, int *ix2, int *ix3,
                                int *ix4, int ityp)
    {

        int i, j, l, ig;

        if (igap == 0)
        {
            for (i = 0; i < jlt; ++i)
            {
                // write the address of gapv[i] to the console
                gapv[i] = std::max(gap + dgapload, drad);
            }
        }
        else if (igap == 3)
        {
            for (i = 0; i < jlt; ++i)
            {
                j = cand_n[i];
                if (j <= nsn)
                {
                    gapv[i] = gap_s[j - 1] + gap_m[cand_e[i] - 1];
                    gapv[i] = std::min(gap_s_l[j - 1] + gap_m_l[cand_e[i] - 1], gapv[i]);
                }
                else
                {
                    ig = j - nsn - 1;
                    gapv[i] = xrem[8 + ig * s_xrem] + gap_m[cand_e[i] - 1];
                    gapv[i] = std::min(xrem[ig * s_xrem + 9] + gap_m_l[cand_e[i] - 1], gapv[i]);
                }
                gapv[i] = std::min(gapv[i], gapmax);
                gapv[i] = std::max(gapmin, gapv[i]);
                gapv[i] = std::max(drad, gapv[i] + dgapload);
            }
        }
        else
        {
            for (i = 0; i < jlt; ++i)
            {
                j = cand_n[i];
                if (j <= nsn)
                {
                    gapv[i] = gap_s[j - 1] + gap_m[cand_e[i] - 1];
                }
                else
                {
                    ig = j - nsn - 1;
                    gapv[i] = xrem[(ig)*s_xrem + 8] + gap_m[cand_e[i] - 1];
                }
                gapv[i] = std::min(gapv[i], gapmax);
                gapv[i] = std::max(gapmin, gapv[i]);
                gapv[i] = std::max(drad, gapv[i] + dgapload);
            }
        }

        for (i = 0; i < jlt; ++i)
        {
            j = cand_n[i] - 1;
            if (j < nsn)
            {
                ig = nsv[j] - 1;
                xi[i] = x[3 * ig];
                yi[i] = x[3 * ig + 1];
                zi[i] = x[3 * ig + 2];
            }
            else
            {
                ig = j - nsn;
                xi[i] = xrem[(ig)*s_xrem];
                yi[i] = xrem[(ig)*s_xrem + 1];
                zi[i] = xrem[(ig)*s_xrem + 2];
            }

            l = cand_e[i] - 1;

            ix1[i] = irect[4 * l];
            x1[i] = x[3 * (ix1[i] - 1) + 0];
            y1[i] = x[3 * (ix1[i] - 1) + 1];
            z1[i] = x[3 * (ix1[i] - 1) + 2];
            ix2[i] = irect[4 * l + 1];
            x2[i] = x[3 * (ix2[i] - 1) + 0];
            y2[i] = x[3 * (ix2[i] - 1) + 1];
            z2[i] = x[3 * (ix2[i] - 1) + 2];
            ix3[i] = irect[4 * l + 2];
            x3[i] = x[3 * (ix3[i] - 1) + 0];
            y3[i] = x[3 * (ix3[i] - 1) + 1];
            z3[i] = x[3 * (ix3[i] - 1) + 2];
            ix4[i] = irect[4 * l + 3];
            x4[i] = x[3 * (ix4[i] - 1) + 0];
            y4[i] = x[3 * (ix4[i] - 1) + 1];
            z4[i] = x[3 * (ix4[i] - 1) + 2];
        }

        if (ityp == 7)
        {
            for (i = 0; i < jlt; ++i)
            {
                gapv[i] = gapv[i] + curv_max[cand_e[i] - 1];
            }
        }
    }
    void cpp_inter7_penetration(
        const int jlt, const my_real margin,
        const my_real *x1, const my_real *x2, const my_real *x3, const my_real *x4,
        const my_real *y1, const my_real *y2, const my_real *y3, const my_real *y4,
        const my_real *z1, const my_real *z2, const my_real *z3, const my_real *z4,
        const my_real *xi, const my_real *yi, const my_real *zi,
        const int *ix3, const int *ix4,
        my_real *pene, const my_real *gapv)
    {
        my_real gap2[GROUP_SIZE];
        my_real x0[GROUP_SIZE], y0[GROUP_SIZE], z0[GROUP_SIZE];
        my_real nx1[GROUP_SIZE], ny1[GROUP_SIZE], nz1[GROUP_SIZE];
        my_real nx2[GROUP_SIZE], ny2[GROUP_SIZE], nz2[GROUP_SIZE];
        my_real nx3[GROUP_SIZE], ny3[GROUP_SIZE], nz3[GROUP_SIZE];
        my_real nx4[GROUP_SIZE], ny4[GROUP_SIZE], nz4[GROUP_SIZE];
        my_real lb1[GROUP_SIZE], lb2[GROUP_SIZE], lb3[GROUP_SIZE], lb4[GROUP_SIZE];
        my_real lc1[GROUP_SIZE], lc2[GROUP_SIZE], lc3[GROUP_SIZE], lc4[GROUP_SIZE];
        my_real al1[GROUP_SIZE], al2[GROUP_SIZE], al3[GROUP_SIZE], al4[GROUP_SIZE];
        my_real hlb1[GROUP_SIZE], hlb2[GROUP_SIZE], hlb3[GROUP_SIZE], hlb4[GROUP_SIZE];
        my_real hlc1[GROUP_SIZE], hlc2[GROUP_SIZE], hlc3[GROUP_SIZE], hlc4[GROUP_SIZE];
        my_real p1[GROUP_SIZE], p2[GROUP_SIZE], p3[GROUP_SIZE], p4[GROUP_SIZE];
        my_real x01[GROUP_SIZE], y01[GROUP_SIZE], z01[GROUP_SIZE];
        my_real x02[GROUP_SIZE], y02[GROUP_SIZE], z02[GROUP_SIZE];
        my_real x03[GROUP_SIZE], y03[GROUP_SIZE], z03[GROUP_SIZE];
        my_real x04[GROUP_SIZE], y04[GROUP_SIZE], z04[GROUP_SIZE];
        my_real xi1[GROUP_SIZE], yi1[GROUP_SIZE], zi1[GROUP_SIZE];
        my_real xi2[GROUP_SIZE], yi2[GROUP_SIZE], zi2[GROUP_SIZE];
        my_real xi3[GROUP_SIZE], yi3[GROUP_SIZE], zi3[GROUP_SIZE];
        my_real xi4[GROUP_SIZE], yi4[GROUP_SIZE], zi4[GROUP_SIZE];
        my_real zoneinf;

        for (int i = 0; i < jlt; ++i)
        {
            zoneinf = gapv[i] + margin; // zone of influence: gap of the element + margin
            gap2[i] = zoneinf * zoneinf;
        }
        int i3n = 0;
        for (int i = 0; i < jlt; ++i)
        {
            if (ix3[i] == ix4[i])
                i3n = i3n + 1;
            if (i3n == jlt)
            {
                i3n = 1;
            }
            else if (i3n != 0)
            {
                i3n = 2;
            }
        }
        //--------------------------------------------------------
        //   quadrangle
        //--------------------------------------------------------
        if (i3n == 0)
        {
            for (int i = 0; i < jlt; ++i)
            {
                x0[i] = fourth * (x1[i] + x2[i] + x3[i] + x4[i]);
                y0[i] = fourth * (y1[i] + y2[i] + y3[i] + y4[i]);
                z0[i] = fourth * (z1[i] + z2[i] + z3[i] + z4[i]);
            }
        }
        //--------------------------------------------------------
        //  triangle
        //--------------------------------------------------------
        else if (i3n == 2)
        {
            for (int i = 0; i < jlt; ++i)
            {
                if (ix3[i] == ix4[i])
                {
                    x0[i] = x3[i];
                    y0[i] = y3[i];
                    z0[i] = z3[i];
                }
                else
                {
                    x0[i] = fourth * (x1[i] + x2[i] + x3[i] + x4[i]);
                    y0[i] = fourth * (y1[i] + y2[i] + y3[i] + y4[i]);
                    z0[i] = fourth * (z1[i] + z2[i] + z3[i] + z4[i]);
                }
            }
        }
        // c--------------------------------------------------------
        // c  triangle
        // c--------------------------------------------------------

        if (i3n == 1)
        {
            for (int i = 0; i < jlt; ++i)
            {
                x01[i] = x1[i] - x3[i];
                y01[i] = y1[i] - y3[i];
                z01[i] = z1[i] - z3[i];
                x02[i] = x2[i] - x3[i];
                y02[i] = y2[i] - y3[i];
                z02[i] = z2[i] - z3[i];

                const my_real xi0 = x3[i] - xi[i];
                const my_real yi0 = y3[i] - yi[i];
                const my_real zi0 = z3[i] - zi[i];
                xi1[i] = x1[i] - xi[i];
                yi1[i] = y1[i] - yi[i];
                zi1[i] = z1[i] - zi[i];
                xi2[i] = x2[i] - xi[i];
                yi2[i] = y2[i] - yi[i];
                zi2[i] = z2[i] - zi[i];
                const my_real sx1 = yi0 * zi1[i] - zi0 * yi1[i];
                const my_real sy1 = zi0 * xi1[i] - xi0 * zi1[i];
                const my_real sz1 = xi0 * yi1[i] - yi0 * xi1[i];
                const my_real sx2 = yi0 * zi2[i] - zi0 * yi2[i];
                const my_real sy2 = zi0 * xi2[i] - xi0 * zi2[i];
                const my_real sz2 = xi0 * yi2[i] - yi0 * xi2[i];
                const my_real sx0 = y01[i] * z02[i] - z01[i] * y02[i];
                const my_real sy0 = z01[i] * x02[i] - x01[i] * z02[i];
                const my_real sz0 = x01[i] * y02[i] - y01[i] * x02[i];
                const my_real s2 = 1. / std::max(em30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);
                lb1[i] = -(sx0 * sx2 + sy0 * sy2 + sz0 * sz2) * s2;
                lc1[i] = (sx0 * sx1 + sy0 * sy1 + sz0 * sz1) * s2;
                my_real aaa = one / std::max(em30, x01[i] * x01[i] + y01[i] * y01[i] + z01[i] * z01[i]);
                hlc1[i] = lc1[i] * std::abs(lc1[i]) * aaa;
                al1[i] = -(xi0 * x01[i] + yi0 * y01[i] + zi0 * z01[i]) * aaa;
                al1[i] = std::max(zero, std::min(one, al1[i]));
                aaa = one / std::max(em30, x02[i] * x02[i] + y02[i] * y02[i] + z02[i] * z02[i]);
                hlb1[i] = lb1[i] * std::abs(lb1[i]) * aaa;
                al2[i] = -(xi0 * x02[i] + yi0 * y02[i] + zi0 * z02[i]) * aaa;
                al2[i] = std::max(zero, std::min(one, al2[i]));
            }

            for (int i = 0; i < jlt; ++i)
            {
                my_real x12 = x2[i] - x1[i];
                my_real y12 = y2[i] - y1[i];
                my_real z12 = z2[i] - z1[i];
                my_real la = one - lb1[i] - lc1[i];
                my_real aaa = one / std::max(em20, x12 * x12 + y12 * y12 + z12 * z12);
                my_real hla = la * std::abs(la) * aaa;
                if (la < zero && hla <= hlb1[i] && hla <= hlc1[i])
                {
                    lb1[i] = (xi2[i] * x12 + yi2[i] * y12 + zi2[i] * z12) * aaa;
                    lb1[i] = std::max(zero, std::min(one, lb1[i]));
                    lc1[i] = one - lb1[i];
                }
                else if (lb1[i] < zero && hlb1[i] <= hlc1[i] && hlb1[i] <= hla)
                {
                    lb1[i] = zero;
                    lc1[i] = al2[i];
                }
                else if (lc1[i] < zero && hlc1[i] <= hla && hlc1[i] <= hlb1[i])
                {
                    lc1[i] = zero;
                    lb1[i] = al1[i];
                }
            }

            for (int i = 0; i < jlt; ++i)
            {
                nx1[i] = xi[i] - (x3[i] + lb1[i] * x01[i] + lc1[i] * x02[i]);
                ny1[i] = yi[i] - (y3[i] + lb1[i] * y01[i] + lc1[i] * y02[i]);
                nz1[i] = zi[i] - (z3[i] + lb1[i] * z01[i] + lc1[i] * z02[i]);
                p1[i] = nx1[i] * nx1[i] + ny1[i] * ny1[i] + nz1[i] * nz1[i];
                pene[i] = std::max(zero, gap2[i] - p1[i]);
            }
        }
        else //
        {

            for (int i = 0; i < jlt; ++i)
            {
                x01[i] = x1[i] - x0[i];
                y01[i] = y1[i] - y0[i];
                z01[i] = z1[i] - z0[i];

                x02[i] = x2[i] - x0[i];
                y02[i] = y2[i] - y0[i];
                z02[i] = z2[i] - z0[i];

                x03[i] = x3[i] - x0[i];
                y03[i] = y3[i] - y0[i];
                z03[i] = z3[i] - z0[i];

                x04[i] = x4[i] - x0[i];
                y04[i] = y4[i] - y0[i];
                z04[i] = z4[i] - z0[i];

                const my_real xi0 = x0[i] - xi[i];
                const my_real yi0 = y0[i] - yi[i];
                const my_real zi0 = z0[i] - zi[i];

                xi1[i] = x1[i] - xi[i];
                yi1[i] = y1[i] - yi[i];
                zi1[i] = z1[i] - zi[i];

                xi2[i] = x2[i] - xi[i];
                yi2[i] = y2[i] - yi[i];
                zi2[i] = z2[i] - zi[i];

                xi3[i] = x3[i] - xi[i];
                yi3[i] = y3[i] - yi[i];
                zi3[i] = z3[i] - zi[i];

                xi4[i] = x4[i] - xi[i];
                yi4[i] = y4[i] - yi[i];
                zi4[i] = z4[i] - zi[i];

                my_real sx1 = yi0 * zi1[i] - zi0 * yi1[i];
                my_real sy1 = zi0 * xi1[i] - xi0 * zi1[i];
                my_real sz1 = xi0 * yi1[i] - yi0 * xi1[i];

                my_real sx2 = yi0 * zi2[i] - zi0 * yi2[i];
                my_real sy2 = zi0 * xi2[i] - xi0 * zi2[i];
                my_real sz2 = xi0 * yi2[i] - yi0 * xi2[i];

                my_real sx0 = y01[i] * z02[i] - z01[i] * y02[i];
                my_real sy0 = z01[i] * x02[i] - x01[i] * z02[i];
                my_real sz0 = x01[i] * y02[i] - y01[i] * x02[i];
                my_real s2 = one / std::max(em30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);

                lb1[i] = -(sx0 * sx2 + sy0 * sy2 + sz0 * sz2) * s2;
                lc1[i] = (sx0 * sx1 + sy0 * sy1 + sz0 * sz1) * s2;

                my_real sx3 = yi0 * zi3[i] - zi0 * yi3[i];
                my_real sy3 = zi0 * xi3[i] - xi0 * zi3[i];
                my_real sz3 = xi0 * yi3[i] - yi0 * xi3[i];

                sx0 = y02[i] * z03[i] - z02[i] * y03[i];
                sy0 = z02[i] * x03[i] - x02[i] * z03[i];
                sz0 = x02[i] * y03[i] - y02[i] * x03[i];
                s2 = one / std::max(em30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);

                lb2[i] = -(sx0 * sx3 + sy0 * sy3 + sz0 * sz3) * s2;
                lc2[i] = (sx0 * sx2 + sy0 * sy2 + sz0 * sz2) * s2;

                my_real sx4 = yi0 * zi4[i] - zi0 * yi4[i];
                my_real sy4 = zi0 * xi4[i] - xi0 * zi4[i];
                my_real sz4 = xi0 * yi4[i] - yi0 * xi4[i];

                sx0 = y03[i] * z04[i] - z03[i] * y04[i];
                sy0 = z03[i] * x04[i] - x03[i] * z04[i];
                sz0 = x03[i] * y04[i] - y03[i] * x04[i];
                s2 = one / std::max(em30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);

                lb3[i] = -(sx0 * sx4 + sy0 * sy4 + sz0 * sz4) * s2;
                lc3[i] = (sx0 * sx3 + sy0 * sy3 + sz0 * sz3) * s2;

                sx0 = y04[i] * z01[i] - z04[i] * y01[i];
                sy0 = z04[i] * x01[i] - x04[i] * z01[i];
                sz0 = x04[i] * y01[i] - y04[i] * x01[i];
                s2 = one / std::max(em30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);

                lb4[i] = -(sx0 * sx1 + sy0 * sy1 + sz0 * sz1) * s2;
                lc4[i] = (sx0 * sx4 + sy0 * sy4 + sz0 * sz4) * s2;

                my_real aaa = one / std::max(em30, x01[i] * x01[i] + y01[i] * y01[i] + z01[i] * z01[i]);
                hlc1[i] = lc1[i] * std::abs(lc1[i]) * aaa;
                hlb4[i] = lb4[i] * std::abs(lb4[i]) * aaa;
                al1[i] = -(xi0 * x01[i] + yi0 * y01[i] + zi0 * z01[i]) * aaa;
                al1[i] = std::max(zero, std::min(one, al1[i]));

                aaa = one / std::max(em30, x02[i] * x02[i] + y02[i] * y02[i] + z02[i] * z02[i]);
                hlc2[i] = lc2[i] * std::abs(lc2[i]) * aaa;
                hlb1[i] = lb1[i] * std::abs(lb1[i]) * aaa;
                al2[i] = -(xi0 * x02[i] + yi0 * y02[i] + zi0 * z02[i]) * aaa;
                al2[i] = std::max(zero, std::min(one, al2[i]));

                aaa = one / std::max(em30, x03[i] * x03[i] + y03[i] * y03[i] + z03[i] * z03[i]);
                hlc3[i] = lc3[i] * std::abs(lc3[i]) * aaa;
                hlb2[i] = lb2[i] * std::abs(lb2[i]) * aaa;
                al3[i] = -(xi0 * x03[i] + yi0 * y03[i] + zi0 * z03[i]) * aaa;
                al3[i] = std::max(zero, std::min(one, al3[i]));

                aaa = one / std::max(em30, x04[i] * x04[i] + y04[i] * y04[i] + z04[i] * z04[i]);
                hlc4[i] = lc4[i] * std::abs(lc4[i]) * aaa;
                hlb3[i] = lb3[i] * std::abs(lb3[i]) * aaa;
                al4[i] = -(xi0 * x04[i] + yi0 * y04[i] + zi0 * z04[i]) * aaa;
                al4[i] = std::max(zero, std::min(one, al4[i]));
            }

            for (int i = 0; i < jlt; ++i)
            {
                my_real x12 = x2[i] - x1[i];
                my_real y12 = y2[i] - y1[i];
                my_real z12 = z2[i] - z1[i];
                my_real la = one - lb1[i] - lc1[i];
                my_real aaa = one / std::max(em20, x12 * x12 + y12 * y12 + z12 * z12);
                my_real hla = la * std::abs(la) * aaa;
                if (la < zero && hla <= hlb1[i] && hla <= hlc1[i])
                {
                    lb1[i] = (xi2[i] * x12 + yi2[i] * y12 + zi2[i] * z12) * aaa;
                    lb1[i] = std::max(zero, std::min(one, lb1[i]));
                    lc1[i] = one - lb1[i];
                }
                else if (lb1[i] < zero && hlb1[i] <= hlc1[i] && hlb1[i] <= hla)
                {
                    lb1[i] = zero;
                    lc1[i] = al2[i];
                }
                else if (lc1[i] < zero && hlc1[i] <= hla && hlc1[i] <= hlb1[i])
                {
                    lc1[i] = zero;
                    lb1[i] = al1[i];
                }
            }

            for (int i = 0; i < jlt; ++i)
            {
                my_real x23 = x3[i] - x2[i];
                my_real y23 = y3[i] - y2[i];
                my_real z23 = z3[i] - z2[i];
                my_real la = one - lb2[i] - lc2[i];
                my_real aaa = one / std::max(em20, x23 * x23 + y23 * y23 + z23 * z23);
                my_real hla = la * std::abs(la) * aaa;
                if (la < zero && hla <= hlb2[i] && hla <= hlc2[i])
                {
                    lb2[i] = (xi3[i] * x23 + yi3[i] * y23 + zi3[i] * z23) * aaa;
                    lb2[i] = std::max(zero, std::min(one, lb2[i]));
                    lc2[i] = one - lb2[i];
                }
                else if (lb2[i] < zero && hlb2[i] <= hlc2[i] && hlb2[i] <= hla)
                {
                    lb2[i] = zero;
                    lc2[i] = al3[i];
                }
                else if (lc2[i] < zero && hlc2[i] <= hla && hlc2[i] <= hlb2[i])
                {
                    lc2[i] = zero;
                    lb2[i] = al2[i];
                }
            }

            for (int i = 0; i < jlt; ++i)
            {
                my_real x34 = x4[i] - x3[i];
                my_real y34 = y4[i] - y3[i];
                my_real z34 = z4[i] - z3[i];
                my_real la = one - lb3[i] - lc3[i];
                my_real aaa = one / std::max(em20, x34 * x34 + y34 * y34 + z34 * z34);
                my_real hla = la * std::abs(la) * aaa;
                if (la < zero && hla <= hlb3[i] && hla <= hlc3[i])
                {
                    lb3[i] = (xi4[i] * x34 + yi4[i] * y34 + zi4[i] * z34) * aaa;
                    lb3[i] = std::max(zero, std::min(one, lb3[i]));
                    lc3[i] = one - lb3[i];
                }
                else if (lb3[i] < zero && hlb3[i] <= hlc3[i] && hlb3[i] <= hla)
                {
                    lb3[i] = zero;
                    lc3[i] = al4[i];
                }
                else if (lc3[i] < zero && hlc3[i] <= hla && hlc3[i] <= hlb3[i])
                {
                    lc3[i] = zero;
                    lb3[i] = al3[i];
                }
            }

            for (int i = 0; i < jlt; ++i)
            {
                my_real x41 = x1[i] - x4[i];
                my_real y41 = y1[i] - y4[i];
                my_real z41 = z1[i] - z4[i];
                my_real la = one - lb4[i] - lc4[i];
                my_real aaa = one / std::max(em20, x41 * x41 + y41 * y41 + z41 * z41);
                my_real hla = la * std::abs(la) * aaa;
                if (la < zero && hla <= hlb4[i] && hla <= hlc4[i])
                {
                    lb4[i] = (xi1[i] * x41 + yi1[i] * y41 + zi1[i] * z41) * aaa;
                    lb4[i] = std::max(zero, std::min(one, lb4[i]));
                    lc4[i] = one - lb4[i];
                }
                else if (lb4[i] < zero && hlb4[i] <= hlc4[i] && hlb4[i] <= hla)
                {
                    lb4[i] = zero;
                    lc4[i] = al1[i];
                }
                else if (lc4[i] < zero && hlc4[i] <= hla && hlc4[i] <= hlb4[i])
                {
                    lc4[i] = zero;
                    lb4[i] = al4[i];
                }
            }

            for (int i = 0; i < jlt; ++i)
            {
                nx1[i] = xi[i] - (x0[i] + lb1[i] * x01[i] + lc1[i] * x02[i]);
                ny1[i] = yi[i] - (y0[i] + lb1[i] * y01[i] + lc1[i] * y02[i]);
                nz1[i] = zi[i] - (z0[i] + lb1[i] * z01[i] + lc1[i] * z02[i]);
                p1[i] = nx1[i] * nx1[i] + ny1[i] * ny1[i] + nz1[i] * nz1[i];
                const my_real d1 = std::max(zero, gap2[i] - p1[i]);
                nx2[i] = xi[i] - (x0[i] + lb2[i] * x02[i] + lc2[i] * x03[i]);
                ny2[i] = yi[i] - (y0[i] + lb2[i] * y02[i] + lc2[i] * y03[i]);
                nz2[i] = zi[i] - (z0[i] + lb2[i] * z02[i] + lc2[i] * z03[i]);
                p2[i] = nx2[i] * nx2[i] + ny2[i] * ny2[i] + nz2[i] * nz2[i];
                const my_real d2 = std::max(zero, gap2[i] - p2[i]);
                nx3[i] = xi[i] - (x0[i] + lb3[i] * x03[i] + lc3[i] * x04[i]);
                ny3[i] = yi[i] - (y0[i] + lb3[i] * y03[i] + lc3[i] * y04[i]);
                nz3[i] = zi[i] - (z0[i] + lb3[i] * z03[i] + lc3[i] * z04[i]);
                p3[i] = nx3[i] * nx3[i] + ny3[i] * ny3[i] + nz3[i] * nz3[i];
                const my_real d3 = std::max(zero, gap2[i] - p3[i]);
                nx4[i] = xi[i] - (x0[i] + lb4[i] * x04[i] + lc4[i] * x01[i]);
                ny4[i] = yi[i] - (y0[i] + lb4[i] * y04[i] + lc4[i] * y01[i]);
                nz4[i] = zi[i] - (z0[i] + lb4[i] * z04[i] + lc4[i] * z01[i]);
                p4[i] = nx4[i] * nx4[i] + ny4[i] * ny4[i] + nz4[i] * nz4[i];
                const my_real d4 = std::max(zero, gap2[i] - p4[i]);
                pene[i] = std::max({d1, d2, d3, d4});
            }
        }
    }



    void cpp_inter7_filter_cand(
        int j_stok, const int *irect, const my_real *x, const int *nsv, int &ii_stok,
        int *cand_n, int *cand_e, int mulnsn, my_real margin,
        int *prov_n, int *prov_e, int eshift, int inacti,
        int ifq, int *cand_a, int nsn,
        const int *oldnum, int nsnrold, int igap, my_real gap, const my_real *gap_s,
        const my_real *gap_m, my_real gapmin, my_real gapmax, const my_real *curv_max,
        const my_real *gap_s_l, const my_real *gap_m_l, my_real drad, int itied,
        my_real dgapload, int nsnr, const my_real *xrem, int s_xrem)
    {

        int i, k_stok, i_stok, n, ne, j;
        int inacti_l, itied_l, ifq_l;
        int j_start, j_end;
        const int itype = 7;
        my_real x1[GROUP_SIZE], x2[GROUP_SIZE], x3[GROUP_SIZE], x4[GROUP_SIZE];
        my_real y1[GROUP_SIZE], y2[GROUP_SIZE], y3[GROUP_SIZE], y4[GROUP_SIZE];
        my_real z1[GROUP_SIZE], z2[GROUP_SIZE], z3[GROUP_SIZE], z4[GROUP_SIZE];
        my_real xi[GROUP_SIZE], yi[GROUP_SIZE], zi[GROUP_SIZE];
        my_real pene[GROUP_SIZE], gapv[GROUP_SIZE];
        int ix1[GROUP_SIZE], ix2[GROUP_SIZE], ix3[GROUP_SIZE], ix4[GROUP_SIZE];




        cpp_inter7_gather_cand(j_stok, x, irect, nsv, prov_e, prov_n, igap, gap, gapv,
                               gap_s, gap_m, curv_max, gapmax, gapmin, dgapload, drad,
                               x1, x2, x3, x4, y1, y2, y3, y4, z1, z2, z3, z4,
                               xi, yi, zi, gap_s_l, gap_m_l, s_xrem, nsn, nsnr,
                               xrem, ix1, ix2, ix3, ix4, itype);

        cpp_inter7_penetration(j_stok, margin, x1, x2, x3, x4,
                               y1, y2, y3, y4, z1, z2, z3, z4,
                               xi, yi, zi, ix3, ix4, pene, gapv);

        if (inacti == 5 || inacti == 6 || inacti == 7 || ifq > 0 || itied != 0)
        {
            for (i = 0; i < j_stok; ++i)
            {
                if (pene[i] != zero)
                {
                    n = prov_n[i];
                    ne = prov_e[i] + eshift;
                    if (n > nsn)
                    {
                        n = oldnum[n - nsn - 1] + nsn;
                        if (n == nsn)
                            n = nsn + nsnrold + 1;
                    }
                    j_start = cand_a[n - 1];
                    j_end = cand_a[n] - 1;
                    for (j = j_start - 1; j < j_end; ++j)
                    {
                        if (cand_e[j] == ne)
                        {
                            pene[i] = zero;
                            break;
                        }
                    }
                }
            }
        }

        k_stok = 0;
        for (i = 0; i < j_stok; ++i)
        {
            if (pene[i] != zero)
                k_stok++;
        }
        if (k_stok == 0)
            return;

#pragma omp critical
        {
            i_stok = ii_stok;
            ii_stok = i_stok + k_stok;
        }

        inacti_l = inacti;
        itied_l = itied;
        ifq_l = ifq;

        for (i = 0; i < j_stok; ++i)
        {
            if (pene[i] != zero)
            {
                //print_address(cand_n[i_stok], std::to_string(__LINE__)+" cand_n");
                cand_n[i_stok] = prov_n[i];
                cand_e[i_stok] = prov_e[i] + eshift;
                //std::cout<<"cand_n["<<i_stok<<"] = "<<cand_n[i_stok]<<"  cand_e["<<i_stok<<"] = "<<cand_e[i_stok]<<std::endl;

                i_stok++;
            }
        }
    }



          void cpp_inter7_candidate_pairs(
                                            int nsn          ,
                                            int * oldnum       ,
                                            int nsnr         ,
                                            int isznsnr      ,
                                            int * i_mem        ,
                                            int * irect        ,
                                            my_real *x            ,
                                            my_real *stf          ,
                                            my_real *stfn         ,
                                            my_real *xyzm         ,
                                            int * nsv          ,
                                            int &ii_stok      ,
                                            int *&cand_n       ,
                                            int eshift       ,
                                            int *& cand_e       ,
                                            int * ncontact       ,
                                            my_real tzinf        ,
                                             my_real * gap_s_l      ,
                                             my_real * gap_m_l      ,
                                            int *voxel        ,
                                            int nbx          ,
                                            int nby          ,
                                            int nbz          ,
                                            int inacti       ,
                                            int ifq          ,
                                            int *cand_a       ,
                                            int nrtm         ,
                                            int nsnrold      ,
                                            int igap         ,
                                            my_real gap          ,
                                            my_real * gap_s        ,
                                            my_real *gap_m        ,
                                            my_real gapmin       ,
                                            my_real gapmax       ,
                                            my_real marge        ,
                                             my_real *curv_max     ,
                                            int itask        ,
                                            my_real bgapsmx      ,
                                            int s_kremnod    ,
                                            int *kremnod      ,
                                            int s_remnod     ,
                                            int *remnod       ,
                                            int flagremnode  ,
                                            my_real drad         ,
                                            int itied        ,
                                            my_real dgapload     ,
                                            int s_cand_a     ,
                                            int total_nb_nrtm,
                                            int numnod       ,
                                             my_real *xrem         ,
                                            int s_xrem       ,
                                             int *irem         ,
                                            int s_irem       ,
                                            int *next_nod    ,
                                            int *cand_n_prev,
                                            int *cand_e_prev) 

    {
        //print address of all arguments
       // print_address(nsn,"LOC(nsn)");
       // print_address(oldnum,"LOC(oldnum)");
       // print_address(nsnr,"LOC(nsnr)");
       // print_address(isznsnr,"LOC(isznsnr)");
       // print_address(i_mem,"LOC(i_mem)");
       // print_address(irect,"LOC(irect)");
       // print_address(x,"LOC(x)");
       // print_address(stf,"LOC(stf)");
       // print_address(stfn,"LOC(stfn)");
       // print_address(xyzm,"LOC(xyzm)");
       // print_address(nsv,"LOC(nsv)");
       // print_address(ii_stok,"LOC(ii_stok)");
       // print_address(cand_n,"LOC(cand_e)");
       // print_address(eshift,"LOC(eshift)");
       // print_address(cand_e,"LOC(cand_e)");
       // print_address(ncontact,"LOC(ncontact)");
       // print_address(tzinf,"LOC(tzinf)");
       // print_address(gap_s_l,"LOC(gap_s_l)");
       // print_address(gap_m_l,"LOC(gap_m_l)");
       // print_address(voxel,"LOC(voxel)");
       // print_address(nbx,"LOC(nbx)");
       // print_address(nby,"LOC(nby)");
       // print_address(nbz,"LOC(nbz)");
       // print_address(inacti,"LOC(inacti)");
       // print_address(ifq,"LOC(ifq)");
       // print_address(cand_a,"LOC(cand_a)");
       // print_address(nrtm,"LOC(nrtm)");
       // print_address(nsnrold,"LOC(nsnrold)");
       // print_address(igap,"LOC(igap)");
       // print_address(gap,"LOC(gap)");
       // print_address(gap_s,"LOC(gap_s)");
       // print_address(gap_m,"LOC(gap_m)");
       // print_address(gapmin,"LOC(gapmin)");
       // print_address(gapmax,"LOC(gapmax)");
       // print_address(marge,"LOC(marge)");
       // print_address(curv_max,"LOC(curv_max)");
       // print_address(itask,"LOC(itask)");
       // print_address(bgapsmx,"LOC(bgapsmx)");
       // print_address(s_kremnod,"LOC(s_kremnod)");
       // print_address(kremnod,"LOC(kremnod)");
       // print_address(s_remnod,"LOC(s_remnod)");
       // print_address(remnod,"LOC(remnod)");
       // print_address(flagremnode,"LOC(flagremnode)");
       // print_address(drad,"LOC(drad)");
       // print_address(itied,"LOC(itied)");
       // print_address(dgapload,"LOC(dgapload)");
       // print_address(s_cand_a,"LOC(s_cand_a)");
       // print_address(total_nb_nrtm,"LOC(total_nb_nrtm)");
       // print_address(numnod,"LOC(numnod)");
       // print_address(xrem,"LOC(xrem)");
       // print_address(s_xrem,"LOC(s_xrem)");
       // print_address(irem,"LOC(irem)");
       // print_address(next_nod,"LOC(next_nod)");

        *i_mem = 0;
        int ncontact_save = *ncontact;
        const int ii_stok_save = ii_stok;

        // The global bounding box contains all the nodes
        // Some nodes may by higly distant from the impact zone
        // The domain is subdivided in cells (voxel)
        // All the cells have the sime size, except the first and the last one in each direction
        // bounding box of the model
        const my_real xmin = xyzm[1 - 1];
        const my_real ymin = xyzm[2 - 1];
        const my_real zmin = xyzm[3 - 1];
        const my_real xmax = xyzm[4 - 1];
        const my_real ymax = xyzm[5 - 1];
        const my_real zmax = xyzm[6 - 1];
        //! reduced bounding box of the model
        //! The reduced bounding box corresponds to voxel(2:nbx+1,2:nby+1,2:nbz+1), it contains cells of the same size
        const my_real xminb = xyzm[7 - 1];
        const my_real yminb = xyzm[8 - 1];
        const my_real zminb = xyzm[9 - 1];
        const my_real xmaxb = xyzm[10 - 1];
        const my_real ymaxb = xyzm[11 - 1];
        const my_real zmaxb = xyzm[12 - 1];
       //  
       //  01234567
       //  |------|       

        // nbx_fine+2 must be a multiple of 8
        size_t nbx_fine =static_cast<size_t>(4 * (nbx +2));  
        size_t nby_fine =static_cast<size_t>(4 * (nby +2));
        size_t nbz_fine =static_cast<size_t>(4 * (nbz +2));
       // std::cout<<"dx="<<(xmaxb - xminb)/static_cast<double>(nbx+2);
       // std::cout<<"dy="<<(ymaxb - yminb)/static_cast<double>(nby+2);
       // std::cout<<"dz="<<(zmaxb - zminb)/static_cast<double>(nbz+2)<<std::endl;
        constexpr size_t numInts = 128*256*256; //64MB
	adjustDimensions(nbx_fine, nby_fine, nbz_fine,numInts);
        VoxelGrid& voxelGrid = VoxelGrid::getInstance(numInts, nbx_fine, nby_fine, nbz_fine);
        voxelGrid.resize(nbx_fine,nby_fine,nbz_fine);
        //if( nbx_fine > 135 && nby_fine > 201 ) std::cout<<"Init value: "<<VoxelGrid.getBit(136,201,39)<<std::endl;
        nbx = static_cast<int>(nbx_fine - 2);
        nby = static_cast<int>(nby_fine - 2);
        nbz = static_cast<int>(nbz_fine - 2);
        // Lambda function to convert 3D indices to 1D index
        auto to1D = [nbx, nby](int i, int j, int k)
        {
            return (i - 1) + (j - 1) * (nbx + 2) + (k - 1) * (nbx + 2) * (nby + 2);
        };


        // start an open single section

#pragma omp barrier

#pragma omp single
        {
            // Allocate the list of voxel with at least one node
            if (total_nb_nrtm > 0)
            {
                for (int i = 1; i <= nsn; i++)
                {
                    int iix = 0;
                    int iiy = 0;
                    int iiz = 0;
                    if (stfn[i - 1] == zero)
                        continue;
                    int j = nsv[i - 1];
                    if (x[0 + (j - 1) * 3] < xmin)
                        continue;
                    if (x[0 + (j - 1) * 3] > xmax)
                        continue;
                    if (x[1 + (j - 1) * 3] < ymin)
                        continue;
                    if (x[1 + (j - 1) * 3] > ymax)
                        continue;
                    if (x[2 + (j - 1) * 3] < zmin)
                        continue;
                    if (x[2 + (j - 1) * 3] > zmax)
                        continue;


                    iix = int(nbx * (x[0 + (j - 1) * 3] - xminb) / (xmaxb - xminb));
                    iiy = int(nby * (x[1 + (j - 1) * 3] - yminb) / (ymaxb - yminb));
                    iiz = int(nbz * (x[2 + (j - 1) * 3] - zminb) / (zmaxb - zminb));
                    iix = std::max(1, 2 + std::min(nbx, iix));
                    iiy = std::max(1, 2 + std::min(nby, iiy));
                    iiz = std::max(1, 2 + std::min(nbz, iiz));
                    //std::cout<<"iix="<<iix<<" iiy="<<iiy<<" iiz="<<iiz<<" i="<<i<<std::endl;

                    const size_t jx = static_cast<size_t>(iix-1);
                    const size_t jy = static_cast<size_t>(iiy-1);
                    const size_t jz = static_cast<size_t>(iiz-1);

                    voxelGrid.addVertexToCell(jx, jy, jz, i);

                }
                for (int i = 1; i <= nsnr; ++i)
                {
                    if (xrem[0 + (i - 1) * s_xrem] < xmin)
                        continue;
                    if (xrem[0 + (i - 1) * s_xrem] > xmax)
                        continue;
                    if (xrem[1 + (i - 1) * s_xrem] < ymin)
                        continue;
                    if (xrem[1 + (i - 1) * s_xrem] > ymax)
                        continue;
                    if (xrem[2 + (i - 1) * s_xrem] < zmin)
                        continue;
                    int iix = int(nbx * (xrem[0 + (i - 1) * s_xrem] - xminb) / (xmaxb - xminb));
                    int iiy = int(nby * (xrem[1 + (i - 1) * s_xrem] - yminb) / (ymaxb - yminb));
                    int iiz = int(nbz * (xrem[2 + (i - 1) * s_xrem] - zminb) / (zmaxb - zminb));
                    iix = std::max(1,2+std::min(nbx, iix));
                    iiy = std::max(1,2+std::min(nby, iiy));
                    iiz = std::max(1,2+std::min(nbz, iiz));

                    //int first = voxel[to1D(iix, iiy, iiz)];
                    int j = nsn + i;
                    const size_t jx = static_cast<size_t>(iix-1);
                    const size_t jy = static_cast<size_t>(iiy-1);
                    const size_t jz = static_cast<size_t>(iiz-1);

                    voxelGrid.addVertexToCell(jx, jy, jz,j);
                }
            }
        } // end of single section

#pragma omp barrier
        int j_stok = 0;
        std::vector<int> prov_n;
        std::vector<int> prov_e;
// parallel for loop schedule dynamic
#pragma omp for schedule(dynamic)
        for (int ne = 0; ne < nrtm; ++ne)
        {

            if (stf[ne] == zero)
                continue; // the segment is deleted/eroded
                          //            if(stf(ne) == zero)cycle ! the segment is deleted/eroded
            my_real aaa = zero;
            if (igap == 0)
            {
                aaa = tzinf + curv_max[ne];
            }
            else
            {
                aaa = marge + curv_max[ne] + std::max(std::min(gapmax, std::max(gapmin, bgapsmx + gap_m[ne])) + dgapload, drad);
            }
            const int m1 = irect[0 + ne * 4];
            const int m2 = irect[1 + ne * 4];
            const int m3 = irect[2 + ne * 4];
            const int m4 = irect[3 + ne * 4];
            const my_real xx1 = x[0 + (m1 - 1) * 3];
            const my_real xx2 = x[0 + (m2 - 1) * 3];
            const my_real xx3 = x[0 + (m3 - 1) * 3];
            const my_real xx4 = x[0 + (m4 - 1) * 3];
            const my_real xmaxe = std::max({xx1, xx2, xx3, xx4});
            const my_real xmine = std::min({xx1, xx2, xx3, xx4});
            const my_real yy1 = x[1 + (m1 - 1) * 3];
            const my_real yy2 = x[1 + (m2 - 1) * 3];
            const my_real yy3 = x[1 + (m3 - 1) * 3];
            const my_real yy4 = x[1 + (m4 - 1) * 3];
            const my_real ymaxe = std::max({yy1, yy2, yy3, yy4});
            const my_real ymine = std::min({yy1, yy2, yy3, yy4});
            const my_real zz1 = x[2 + (m1 - 1) * 3];
            const my_real zz2 = x[2 + (m2 - 1) * 3];
            const my_real zz3 = x[2 + (m3 - 1) * 3];
            const my_real zz4 = x[2 + (m4 - 1) * 3];
            const my_real zmaxe = std::max({zz1, zz2, zz3, zz4});
            const my_real zmine = std::min({zz1, zz2, zz3, zz4});
            const my_real sx = (yy3 - yy1) * (zz4 - zz2) - (zz3 - zz1) * (yy4 - yy2);
            const my_real sy = (zz3 - zz1) * (xx4 - xx2) - (xx3 - xx1) * (zz4 - zz2);
            const my_real sz = (xx3 - xx1) * (yy4 - yy2) - (yy3 - yy1) * (xx4 - xx2);
            const my_real s2 = sx * sx + sy * sy + sz * sz;
            size_t ix1, ix2, iy1, iy2, iz1, iz2 ;

            compute_coordinates(nbx, xmaxb, xminb, ix1, xmine, aaa, ix2, xmaxe, nby, ymaxb, yminb, iy1, ymine, iy2, ymaxe, nbz, zmaxb, zminb, iz1, zmine, iz2, zmaxe);


            //std::cout<<"ix1 = "<<ix1<<" ix2 = "<<ix2<<" iy1 = "<<iy1<<" iy2 = "<<iy2<<" iz1 = "<<iz1<<" iz2 = "<<iz2<<std::endl;
            for (size_t iz = iz1; iz <= iz2; ++iz)
            {
                for (size_t iy = iy1; iy <= iy2; ++iy)
                {
                    for (size_t ix = ix1; ix <= ix2; ++ix)
                    {
                        //int jj = voxel[to1D(ix, iy, iz)];
                        //for (; jj != 0; jj = next_nod[jj - 1])
                        if( voxelGrid.getBit(ix, iy, iz))
                        {
                           for(const auto& jj : voxelGrid.getCell(ix, iy, iz))
                           {

                            my_real xs = zero;
                            my_real ys = zero;
                            my_real zs = zero;
                            if (jj <= nsn) // local node
                            {
                                const int nn = nsv[jj - 1];
                                if (nn == m1)
                                    continue;
                                if (nn == m2)
                                    continue;
                                if (nn == m3)
                                    continue;
                                if (nn == m4)
                                    continue;

                                xs = x[0 + (nn - 1) * 3];
                                ys = x[1 + (nn - 1) * 3];
                                zs = x[2 + (nn - 1) * 3];
                                if (igap != 0)
                                {
                                    aaa = marge + curv_max[ne] + std::max(std::min(gapmax, std::max(gapmin, bgapsmx + gap_m[ne])) + dgapload, drad);
                                }
                            }
                            else // remote spmd node
                            {
                                const int j = jj - nsn;
                                xs = xrem[0 + (j - 1) * s_xrem];
                                ys = xrem[1 + (j - 1) * s_xrem];
                                zs = xrem[2 + (j - 1) * s_xrem];
                                if (igap != 0)
                                {
                                    aaa = marge + curv_max[ne] + std::max(std::min(gapmax, std::max(gapmin, xrem[8 + (j - 1) * s_xrem] + gap_m[ne])) + dgapload, drad);
                                }
                            }

                           // std::cout<<"xs = "<<xs<<" ys = "<<ys<<" zs = "<<zs<<std::endl;
                           // std::cout<<"xmin = "<<xmin<<" xmax = "<<xmax<<" ymin = "<<ymin<<" ymax = "<<ymax<<" zmin = "<<zmin<<" zmax = "<<zmax<<std::endl;
                           // std::cout<<"aaa = "<<aaa<<std::endl;

                            if (xs <= xmine - aaa)
                                continue;
                            if (xs >= xmaxe + aaa)
                                continue;
                            if (ys <= ymine - aaa)
                                continue;
                            if (ys >= ymaxe + aaa)
                                continue;
                            if (zs <= zmine - aaa)
                                continue;
                            if (zs >= zmaxe + aaa)
                                continue;

                            // check for wrapped main segment
                            const my_real d1x = xs - xx1;
                            const my_real d1y = ys - yy1;
                            const my_real d1z = zs - zz1;
                            const my_real d2x = xs - xx2;
                            const my_real d2y = ys - yy2;
                            const my_real d2z = zs - zz2;
                            const my_real dd1 = d1x * sx + d1y * sy + d1z * sz;
                            const my_real dd2 = d2x * sx + d2y * sy + d2z * sz;
                            if (dd1 * dd2 > zero)
                            {
                                const my_real d2 = std::min(dd1 * dd1, dd2 * dd2);
                                const my_real a2 = aaa * aaa * s2;
                                if (d2 > a2)
                                    continue;
                            }
                            j_stok++;
                            //std::cout<<"j_stok = "<<j_stok<<" jj = "<<jj<<" ne = "<<ne<<std::endl;                                                    
                            prov_n.push_back(jj);
                            prov_e.push_back(ne+1);
                           }
                        } // jj
                    } // ix
                } // iy
            } // iz
        } // end of parallel for loop


        if (itask == 0)
        {
              cand_n= static_cast<int*>(std::malloc((ii_stok_save+j_stok) * sizeof(int)));
              if(cand_n == nullptr)
              {
                  std::cout<<"allocation failed"<<std::endl;
              }
              cand_e= static_cast<int*>(std::malloc((ii_stok_save+j_stok) * sizeof(int)));
              //std::cout<<"allocation of size "<<j_stok<<std::endl;
              if(cand_e == nullptr)
              {
                  std::cout<<"allocation failed"<<std::endl;
              }
              // fill with zeros
              for(int i = ii_stok_save; i < ii_stok_save+j_stok ; ++i)
              {
                  cand_n[i] = 0;
                  cand_e[i] = 0;
              }
              for(int i = 0 ; i < ii_stok_save ; ++i)
              {
                  cand_n[i] = cand_n_prev[i];
                  cand_e[i] = cand_e_prev[i];
              }
        }
#pragma omp barrier

            int *prov_n_array = new int[GROUP_SIZE];
            int *prov_e_array = new int[GROUP_SIZE];



        for(int i = 0 ; i < j_stok ; i+= GROUP_SIZE)
        {
//    void cpp_inter7_filter_cand(
//        int j_stok, const int *irect, const my_real *x, const int *nsv, int &ii_stok,
//        int *cand_n, int *cand_e, int mulnsn, my_real margin,
//        int *prov_n, int *prov_e, int eshift, int inacti,
//        int ifq, int *cand_a, my_real *cand_p, int *ifpen, int nsn,
//        const int *oldnum, int nsnrold, int igap, my_real gap, const my_real *gap_s,
//        const my_real *gap_m, my_real gapmin, my_real gapmax, const my_real *curv_max,
//        const my_real *gap_s_l, const my_real *gap_m_l, my_real drad, int itied,
//        my_real *cand_f, my_real dgapload, int nsnr, const my_real *xrem, int s_xrem)
//    {
            //copy prov_n.data into int * array
            int j = std::min(GROUP_SIZE, j_stok - i);
            // points to prov_n[i]
            for(int k = 0 ; k < j ; ++k)
            {
                prov_n_array[k] = prov_n[i+k];
                prov_e_array[k] = prov_e[i+k];
            }
            cpp_inter7_filter_cand(
                j, irect, x, nsv, ii_stok,
                cand_n, cand_e, *ncontact, marge,
                prov_n_array, prov_e_array, eshift, inacti,
                ifq, cand_a,  nsn,
                oldnum, nsnrold, igap, gap, gap_s,
                gap_m, gapmin, gapmax, curv_max,
                gap_s_l, gap_m_l, drad, itied,
                dgapload,
                nsnr, xrem, s_xrem);
        }

//        print_address(cand_n, std::to_string(__LINE__)+" cand_n");

        delete[] prov_n_array;
        delete[] prov_e_array;

        voxelGrid.clear();

//#pragma omp single
//        {
//            for(int i = 0 ; i < ii_stok ; ++i)
//            {
//                std::cout<<"cand_n["<<i<<"] = "<<cand_n[i]<<" cand_e["<<i<<"] = "<<cand_e[i]<<std::endl;
//            }
//        }


//        print_address(cand_n,"LOC(cand_n)");
//        print_address(cand_e,"LOC(cand_e)");
//          if(ii_stok > 0) std::cout<<"cand_e[0] = "<<cand_e[0]<<" cand_n[0] = "<<cand_n[0]<<std::endl;


    } // end of cpp_inter7_candidate_pairs

}
