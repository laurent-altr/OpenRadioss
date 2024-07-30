#include <algorithm>
#include <cstddef>
#include <iostream>

extern "C" {
    typedef double my_real;

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
                            int *ix4, int ityp) {
        
        int i, j, l, ig;

        if (igap == 0) {
            for (i = 0; i < jlt; ++i) {
                // write the address of gapv[i] to the console
                //std::cout<<i<<" "<<gapv[i] << &gapv[i] << std::endl;
                gapv[i] = std::max(gap + dgapload, drad);
            }
        } else if (igap == 3) {
            for (i = 0; i < jlt; ++i) {
                j = cand_n[i];
                if (j <= nsn) {
                    gapv[i] = gap_s[j-1] + gap_m[cand_e[i]-1];
                    gapv[i] = std::min(gap_s_l[j-1] + gap_m_l[cand_e[i]-1], gapv[i]);
                } else {
                    ig = j - nsn -1;
                    gapv[i] = xrem[8 + ig*s_xrem] + gap_m[cand_e[i]-1];
                    gapv[i] = std::min(xrem[ig*s_xrem+9] + gap_m_l[cand_e[i]-1], gapv[i]);
                }
                gapv[i] = std::min(gapv[i], gapmax);
                gapv[i] = std::max(gapmin, gapv[i]);
                gapv[i] = std::max(drad, gapv[i] + dgapload);
            }
        } else {
            for (i = 0; i < jlt; ++i) {
                j = cand_n[i];
                if (j <= nsn) {
                    gapv[i] = gap_s[j-1] + gap_m[cand_e[i]-1];
                } else {
                    ig = j - nsn -1;
                    gapv[i] = xrem[(ig)*s_xrem + 8] + gap_m[cand_e[i]-1];
                }
                gapv[i] = std::min(gapv[i], gapmax);
                gapv[i] = std::max(gapmin, gapv[i]);
                gapv[i] = std::max(drad, gapv[i] + dgapload);
            }
        }

        for (i = 0; i < jlt; ++i) {
            j = cand_n[i] - 1;
            if (j < nsn) {
                ig = nsv[j]-1;
                xi[i] = x[3 * ig];
                yi[i] = x[3 * ig + 1];
                zi[i] = x[3 * ig + 2];
            } else {
                ig = j - nsn; 
                xi[i] = xrem[(ig)*s_xrem];
                yi[i] = xrem[(ig)*s_xrem + 1];
                zi[i] = xrem[(ig)*s_xrem + 2];
            }

            l = cand_e[i] - 1;

            ix1[i] = irect[4 * l];
            x1[i] = x[3 *(ix1[i]-1) + 0];
            y1[i] = x[3 *(ix1[i]-1) + 1];
            z1[i] = x[3 *(ix1[i]-1) + 2];
            ix2[i] = irect[4 * l + 1];
            x2[i] = x[3 *(ix2[i]-1) + 0];
            y2[i] = x[3 *(ix2[i]-1) + 1];
            z2[i] = x[3 *(ix2[i]-1) + 2];
            ix3[i] = irect[4 * l + 2];
            x3[i] = x[3 *(ix3[i]-1) + 0];
            y3[i] = x[3 *(ix3[i]-1) + 1];
            z3[i] = x[3 *(ix3[i]-1) + 2];
            ix4[i] = irect[4 * l + 3];
            x4[i] = x[3 *(ix4[i]-1) + 0];
            y4[i] = x[3 *(ix4[i]-1) + 1];
            z4[i] = x[3 *(ix4[i]-1) + 2];
        }

        if (ityp == 7) {
            for (i = 0; i < jlt; ++i) {
                gapv[i] = gapv[i] + curv_max[cand_e[i]-1];
            }
        }
    }
}
