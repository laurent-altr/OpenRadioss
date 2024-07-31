#include <algorithm>
#include <cstddef>
#include <iostream>

#include <cmath>

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

extern "C"
{
    void cpp_inter7_gather_cand(int jlt, my_real *x, int *irect, int *nsv, int *cand_e,
                                int *cand_n, int igap, my_real gap, my_real *gapv,
                                my_real *gap_s, my_real *gap_m, my_real *curv_max,
                                my_real gapmax, my_real gapmin, my_real dgapload,
                                my_real drad, my_real *x1, my_real *x2, my_real *x3,
                                my_real *x4, my_real *y1, my_real *y2, my_real *y3,
                                my_real *y4, my_real *z1, my_real *z2, my_real *z3,
                                my_real *z4, my_real *xi, my_real *yi, my_real *zi,
                                my_real *gap_s_l, my_real *gap_m_l, int s_xrem, int nsn,
                                int nsnr, my_real *xrem, int *ix1, int *ix2, int *ix3,
                                int *ix4, int ityp)
    {

        int i, j, l, ig;

        if (igap == 0)
        {
            for (i = 0; i < jlt; ++i)
            {
                // write the address of gapv[i] to the console
                // std::cout<<i<<" "<<gapv[i] << &gapv[i] << std::endl;
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

//        std::cout<<"gapv[0] = "<<gapv[0]<<std::endl;
//        std::cout<<"pene[0] = "<<pene[0]<<std::endl;
//        std::cout<<"ix3[0] = "<<ix3[0]<<std::endl;
//        std::cout<<"ix4[0] = "<<ix4[0]<<std::endl;
//        std::cout<<"margin = "<<margin<<std::endl;

        for (int i = 0; i < jlt; ++i)
        {
//            std::cout<<"gapv["<<i<<"] = "<<gapv[i]<<" margin = "<<margin<<std::endl;
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
               nx1[i] = xi[i]-(x0[i] + lb1[i]*x01[i] + lc1[i]*x02[i]);
               ny1[i] = yi[i]-(y0[i] + lb1[i]*y01[i] + lc1[i]*y02[i]);
               nz1[i] = zi[i]-(z0[i] + lb1[i]*z01[i] + lc1[i]*z02[i]);
               p1[i] = nx1[i]*nx1[i] + ny1[i]*ny1[i] +nz1[i]*nz1[i];
              // std::cout<<"xi["<<i<<"] = "<<xi[i]<<std::endl;
              // std::cout<<"yi["<<i<<"] = "<<yi[i]<<std::endl;
              // std::cout<<"zi["<<i<<"] = "<<zi[i]<<std::endl;
              // std::cout<<"x0["<<i<<"] = "<<x0[i]<<std::endl;
              // std::cout<<"lc1["<<i<<"] = "<<lc1[i]<<std::endl;
              // std::cout<<"p["<<i<<"] = "<<p1[i]<<std::endl;
              // write(6,* "gap2(",i,")=",gap2(i)
              //std::cout<<"gap2["<<i<<"] = "<<gap2[i]<<std::endl;

               const my_real d1 = std::max(zero, gap2[i] - p1[i]);
               nx2[i] = xi[i]-(x0[i] + lb2[i]*x02[i] + lc2[i]*x03[i]);
               ny2[i] = yi[i]-(y0[i] + lb2[i]*y02[i] + lc2[i]*y03[i]);
               nz2[i] = zi[i]-(z0[i] + lb2[i]*z02[i] + lc2[i]*z03[i]);
               p2[i] = nx2[i]*nx2[i] + ny2[i]*ny2[i] +nz2[i]*nz2[i];
               const my_real d2 = std::max(zero, gap2[i] - p2[i]);
               nx3[i] = xi[i]-(x0[i] + lb3[i]*x03[i] + lc3[i]*x04[i]);
               ny3[i] = yi[i]-(y0[i] + lb3[i]*y03[i] + lc3[i]*y04[i]);
               nz3[i] = zi[i]-(z0[i] + lb3[i]*z03[i] + lc3[i]*z04[i]);
               p3[i] = nx3[i]*nx3[i] + ny3[i]*ny3[i] +nz3[i]*nz3[i];
               const my_real d3 = std::max(zero, gap2[i] - p3[i]);
               nx4[i] = xi[i]-(x0[i] + lb4[i]*x04[i] + lc4[i]*x01[i]);
               ny4[i] = yi[i]-(y0[i] + lb4[i]*y04[i] + lc4[i]*y01[i]);
               nz4[i] = zi[i]-(z0[i] + lb4[i]*z04[i] + lc4[i]*z01[i]);
               p4[i] = nx4[i]*nx4[i] + ny4[i]*ny4[i] +nz4[i]*nz4[i];
               const my_real d4 = std::max(zero, gap2[i] - p4[i]);
               pene[i] = std::max({d1,d2,d3,d4}); 
               //std::cout<<"pene["<<i<<"] = "<<pene[i]<<std::endl;
            }
        }
    }
}