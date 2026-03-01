//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
//
// sigeps36c.cpp  --  C++ translation of sigeps36c.F
//   called by  : mulawc.F90
//   calls      : vinter_  (vinter.F)
//                vinter2_ (vinter.F)
//
//====================================================================

#include <cmath>
#include <algorithm>
#include <cstring>

// ---------------------------------------------------------------------------
// MVSIZ  (equivalent of mvsiz_p.inc: compile-time constant, default 33)
// ---------------------------------------------------------------------------
#ifndef MVSIZ
#define MVSIZ 33
#endif

// ---------------------------------------------------------------------------
// my_real
// ---------------------------------------------------------------------------
using my_real = double;

// ---------------------------------------------------------------------------
// Mathematical constants  (equivalent of USE CONSTANT_MOD)
// ---------------------------------------------------------------------------
static constexpr my_real ZERO          = 0.0;
static constexpr my_real ONE           = 1.0;
static constexpr my_real TWO           = 2.0;
static constexpr my_real THREE         = 3.0;
static constexpr my_real FOUR          = 4.0;
static constexpr my_real FIVE          = 5.0;
static constexpr my_real HALF          = 0.5;
static constexpr my_real THIRD         = 1.0 / 3.0;
static constexpr my_real FOURTH        = 0.25;
static constexpr my_real TWO_THIRD     = 2.0 / 3.0;
static constexpr my_real THREE_HALF    = 1.5;
static constexpr my_real THREE_OVER_4  = 0.75;
static constexpr my_real FOUR_OVER_5   = 0.8;
static constexpr my_real EM20          = 1.0e-20;
static constexpr my_real EP20          = 1.0e+20;

// ---------------------------------------------------------------------------
// COMMON /PARAM/ -- only the first 41 integers are needed (up to NPROPMI).
// The Fortran block name is /PARAM/, GFortran symbol: param_
// ---------------------------------------------------------------------------
extern "C"
{
    struct param_t
    {
        int npropm, nvsiz, npropg, nparg, lveul,
            nixfr1, nixfr2,
            npari, lwamp, lwanmp, nisx, nigr,
            nisu, nimv, nnpby, nrby, nifv,
            nthvki, npsav, nrcnx, nr2r, nrwlp,
            nrvolu, nbvelp, nrivf,
            nxframe, lactiv, nicbag, nrcbag, nibjet,
            nrbjet, nibhol, nrbhol, lkjni, lkjnr,
            ngjoint, nummpc, nnprw, npebc, npropgi,
            npropmi;   // position 41 – leading dimension of IPM
    };
    extern param_t param_;
}

// ---------------------------------------------------------------------------
// COMMON /IMPL1/ -- integers up to IKT (position 60).
// GFortran symbol: impl1_
// ---------------------------------------------------------------------------
extern "C"
{
    struct impl1_t
    {
        // positions 1-6
        int impl_s, isolv, iprec, itol, nitol, maxb;
        // 7-13
        int iline, isprb, imconv, isetk, n_lim, l_lim, nexp;
        // 14-20
        int ikg, ikpat, nnsiz, lprint, nprint, max_l, insolv;
        // 21-28
        int idtc, idyna, nl_dtp, nl_dtn, impdeb, idsc, maxb0, impmv;
        // 29-36
        int imp_rby, imp_int, isprn, imumpsd, nddl_l, isigini, idsgap, ilintf;
        // 37-43
        int intp_c, l_bfgs, irref, impl_s0, n_pat, maxb1, imp_chk;
        // 44-50
        int imp_iw, imp_ir, idy_damp, iqstat, ndeb0, ndeb1, imp_int7;
        // 51-57
        int nt_imp1, ittoff, nddli_g, ibuckl, iautspc, iscau, itrmax;
        // 58-60
        int imp_lr, lmemv, ikt;
    };
    extern impl1_t impl1_;
}

// ---------------------------------------------------------------------------
// External Fortran interpolation routines (from vinter.F)
// ---------------------------------------------------------------------------
extern "C"
{
    void vinter_(my_real* tf, int* iad, int* ipos, int* ilen, int* nel,
                 my_real* x, my_real* dydx, my_real* y);
    void vinter2_(my_real* tf, int* iad, int* ipos, int* ilen, int* nel,
                  my_real* x, my_real* dydx, my_real* y);
}

// ---------------------------------------------------------------------------
// Helper: Fortran 2-D column-major element access macros
//   UVAR(I,J)   -> uvar[J*nel + I]   (0-based I,J; leading dim = nel)
//   VARTMP(I,J) -> vartmp[J*nel + I] (0-based I,J; leading dim = nel)
//   SIGNOR(I,J) -> signor[J*MVSIZ+I] (0-based I,J; leading dim = MVSIZ)
//   YFAC(I,J)   -> yfac[J*MVSIZ+I]  (0-based I,J; leading dim = MVSIZ)
//   IPM(r,c)    -> ipm[(c-1)*npropmi + (r-1)]  (1-based r,c)
// ---------------------------------------------------------------------------

// ===========================================================================
//  sigeps36c_  -- entry point called from Fortran (mulawc.F90)
//
//  All scalar Fortran arguments arrive as pointers (pass-by-reference).
//  Array arguments are flat pointers; 2-D arrays are column-major.
// ===========================================================================
extern "C"
void sigeps36c_(
    int*     nel_p,
    int*     nuvar_p,
    int*     nvartmp_p,
    int*     mfunc_p,
    int*     kfunc,       // (MFUNC)  – 1-based function indices
    int*     npf,         // (*)      – function descriptor array
    int*     iplas,       // (*)      – plastic integration flag(s)
    my_real* tf,          // (*)      – tabulated function data
    my_real* timestep,
    my_real* uparam,      // (*)      – material parameter array (1-based in Fortran)
    my_real* rho0,        // (NEL)
    my_real* thkly,       // (NEL)
    int*     israte_p,
    my_real* asrate,
    my_real* epsd_pg,     // (NEL)
    my_real* epsd,        // (NEL)
    my_real* epspxx,      // (NEL)
    my_real* epspyy,      // (NEL)
    my_real* epspxy,      // (NEL)
    my_real* depsxx,      // (NEL)
    my_real* depsyy,      // (NEL)
    my_real* depsxy,      // (NEL)
    my_real* depsyz,      // (NEL)
    my_real* depszx,      // (NEL)
    my_real* epsxx,       // (NEL)
    my_real* epsyy,       // (NEL)
    my_real* epsxy,       // (NEL)
    my_real* sigoxx,      // (NEL)
    my_real* sigoyy,      // (NEL)
    my_real* sigoxy,      // (NEL)
    my_real* sigoyz,      // (NEL)
    my_real* sigozx,      // (NEL)
    my_real* signxx,      // (NEL)
    my_real* signyy,      // (NEL)
    my_real* signxy,      // (NEL)
    my_real* signyz,      // (NEL)
    my_real* signzx,      // (NEL)
    my_real* soundsp,     // (NEL)
    my_real* viscmax,     // (NEL)
    my_real* thk,         // (NEL)
    my_real* pla,         // (NEL)
    my_real* uvar,        // (NEL,NUVAR)  column-major, leading dim = nel
    int*     vartmp,      // (NEL,NVARTMP) column-major, leading dim = nel
    my_real* off,         // (NEL)
    int*     ipm,         // (NPROPMI,*)  column-major, leading dim = npropmi
    int*     imat_p,
    my_real* etse,        // (NEL)
    my_real* gs,          // (NEL)
    my_real* yld,         // (NEL)
    my_real* dpla_i,      // (NEL)
    my_real* gama_imp,    // (NEL)
    my_real* signor,      // (MVSIZ,5) column-major, leading dim = MVSIZ
    my_real* shf,         // (NEL)
    my_real* hardm,       // (NEL)
    my_real* facyldi,     // (NEL)
    int*     inloc_p,
    my_real* dplanl,      // (NEL)
    my_real* dmg,         // (NEL)
    my_real* planl,       // (NEL*L_PLANL)
    int*     l_planl_p,
    my_real* sigbxx,      // (NEL)
    my_real* sigbyy,      // (NEL)
    my_real* sigbxy,      // (NEL)
    my_real* loff         // (NEL)
)
{
    // ------------------------------------------------------------------
    // Dereference scalar pointers once
    // ------------------------------------------------------------------
    const int nel     = *nel_p;
    const int nuvar   = *nuvar_p;
    const int israte  = *israte_p;
    const int inloc   = *inloc_p;
    const int imat    = *imat_p;       // 1-based material index

    // NPROPMI from COMMON /PARAM/
    const int npropmi = param_.npropmi;

    // IMPL_S, IKT from COMMON /IMPL1/
    const int impl_s  = impl1_.impl_s;
    const int ikt     = impl1_.ikt;

    // NITER (DATA NITER/3/ in Fortran)
    static const int niter = 3;

    // ------------------------------------------------------------------
    // Local working arrays (MVSIZ-sized, matching Fortran stack arrays)
    // ------------------------------------------------------------------
    int    iad1[MVSIZ], ipos1[MVSIZ], ilen1[MVSIZ];
    int    iad2[MVSIZ], ipos2[MVSIZ], ilen2[MVSIZ];
    int    jj[MVSIZ], index_arr[MVSIZ], iposp[MVSIZ], ipospe[MVSIZ];
    int    ifunc[100], iadp[MVSIZ], ilenp[MVSIZ];
    int    index_pla[MVSIZ];

    my_real fact[MVSIZ], e_arr[MVSIZ], a1[MVSIZ], a2[MVSIZ], g_arr[MVSIZ];
    my_real dydx1[MVSIZ], dydx2[MVSIZ], rfac[MVSIZ];
    my_real y1[MVSIZ], y2[MVSIZ], dr[MVSIZ];
    // YFAC(MVSIZ,2) – column-major, leading dim = MVSIZ
    my_real yfac[2 * MVSIZ];           // yfac[j*MVSIZ + i]  j in {0,1}
    my_real escale[MVSIZ];
    my_real aa[MVSIZ], bb[MVSIZ], dpla_j[MVSIZ];
    my_real pp[MVSIZ], qq[MVSIZ], fail[MVSIZ], h[MVSIZ], hk[MVSIZ];
    my_real sigexx[MVSIZ], sigeyy[MVSIZ], sigexy[MVSIZ];
    my_real svm2[MVSIZ], yld2[MVSIZ], hi[MVSIZ], g3[MVSIZ], epst[MVSIZ];
    my_real pfac[MVSIZ], p0[MVSIZ], dfdp[MVSIZ], pscale[MVSIZ];
    my_real dydxe[MVSIZ], plap[MVSIZ];
    my_real tab_loc[MVSIZ];

    // FACT is declared as FACT(NEL) in Fortran, but NEL <= MVSIZ always.
    // We reuse fact[] above (size MVSIZ).

    // ------------------------------------------------------------------
    // Lambda-style helper: access 2-D column-major YFAC
    //   yfac_ref(i, j)  i in [0,nel), j in {0,1}
    // ------------------------------------------------------------------
    auto yfac_ref = [&](int i, int j) -> my_real& {
        return yfac[j * MVSIZ + i];
    };
    // VARTMP(I,J) with 1-based I,J: vartmp[(J-1)*nel + (I-1)]
    auto vartmp_ref = [&](int i1, int j1) -> int& {
        return vartmp[(j1 - 1) * nel + (i1 - 1)];
    };
    // IPM(row,col) 1-based
    auto ipm_ref = [&](int r, int c) -> int {
        return ipm[(c - 1) * npropmi + (r - 1)];
    };
    // SIGNOR(I,J) 1-based (leading dim MVSIZ, 0-based internally)
    auto signor_ref = [&](int i1, int j1) -> my_real& {
        return signor[(j1 - 1) * MVSIZ + (i1 - 1)];
    };
    // UVAR(I,J) 1-based (leading dim nel)
    auto uvar_ref = [&](int i1, int j1) -> my_real& {
        return uvar[(j1 - 1) * nel + (i1 - 1)];
    };

    // ======================================================================
    //  Initialisation – read material parameters
    // ======================================================================
    // IPM(10,IMAT)  and  IPM(10+J,IMAT)
    int nfunc_mat = ipm_ref(10, imat);
    for (int j = 1; j <= nfunc_mat; ++j) {
        ifunc[j] = ipm_ref(10 + j, imat);   // 1-based storage, matches Fortran
    }
    int ipfun = ifunc[nfunc_mat - 1];

    // Scalar material parameters (uparam is 1-based, subtract 1 for C)
    const my_real e1     = uparam[2 - 1];
    const my_real a11    = uparam[3 - 1];
    const my_real a21    = uparam[4 - 1];
    const my_real g1     = uparam[5 - 1];
    const my_real nux    = uparam[6 - 1];

    const int     nrate    = (int)std::round(uparam[1 - 1]);
    const my_real epsmax   = uparam[2 * nrate + 7 - 1];
    const my_real epsr1    = uparam[2 * nrate + 8 - 1];
    const my_real epsr2    = uparam[2 * nrate + 9 - 1];
    const my_real g31      = uparam[2 * nrate + 11 - 1];
    const my_real fisokin  = uparam[2 * nrate + 14 - 1];
    const my_real epsf     = uparam[2 * nrate + 15 - 1];
    const int     pfun     = (int)std::round(uparam[2 * nrate + 16 - 1]);
    const my_real soundsp1 = uparam[2 * nrate + 18 - 1];
    const my_real nu_mnu   = uparam[2 * nrate + 19 - 1];  // NU/(1-NU)
    const my_real t_pnu    = uparam[2 * nrate + 20 - 1];  // 3/(1+NU)
    const my_real u_mnu    = uparam[2 * nrate + 21 - 1];  // 1/(1-NU)
    const my_real opte_r   = uparam[2 * nrate + 23 - 1];
    const my_real einf     = uparam[2 * nrate + 24 - 1];
    const my_real ce       = uparam[2 * nrate + 25 - 1];
    const int     vp       = (int)std::round(uparam[2 * nrate + 26 - 1]);
    const int     ifail    = (int)std::round(uparam[2 * nrate + 27 - 1]);
    const int     yldcheck = (int)std::round(uparam[2 * nrate + 28 - 1]);
    const int     ismooth  = (int)std::round(uparam[2 * nrate + 29 - 1]);

    const int opte = (int)opte_r;

    // ======================================================================
    //  Initialise element arrays
    // ======================================================================
    int nindex_pla = 0;

    for (int i = 0; i < nel; ++i) {
        viscmax[i] = ZERO;
        etse[i]    = ONE;
        e_arr[i]   = e1;
        a1[i]      = a11;
        a2[i]      = a21;
        g_arr[i]   = g1;
        g3[i]      = g31;
        soundsp[i] = soundsp1;
    }

    // ======================================================================
    //  Variable Young's modulus
    // ======================================================================
    if (opte == 1) {
        // Tabulated Young's modulus
        int ifunce = (int)uparam[2 * nrate + 22 - 1];
        if (ifunce > 0) {
            for (int i = 0; i < nel; ++i) {
                if (pla[i] > ZERO) {
                    int ii               = nindex_pla;
                    index_pla[ii]        = i;      // 0-based element index
                    ipospe[ii]           = vartmp_ref(i + 1, 1);
                    iadp[ii]             = npf[kfunc[ifunce] - 1] / 2 + 1;
                    ilenp[ii]            = npf[kfunc[ifunce]] / 2
                                           - iadp[ii] - ipospe[ii];
                    tab_loc[ii]          = pla[i];
                    ++nindex_pla;
                }
            }
            // Call Fortran VINTER2
            vinter2_(tf, iadp, ipospe, ilenp, &nindex_pla,
                     tab_loc, dydxe, escale);

            // Write back IPOSPE into VARTMP(:,1)
            for (int ii = 0; ii < nindex_pla; ++ii) {
                vartmp_ref(index_pla[ii] + 1, 1) = ipospe[ii];
            }
        }

        for (int ii = 0; ii < nindex_pla; ++ii) {
            int i      = index_pla[ii];
            e_arr[i]   = escale[ii] * e1;
            a1[i]      = e_arr[i] / (ONE - nux * nux);
            a2[i]      = nux * a1[i];
            g_arr[i]   = HALF * e_arr[i] / (ONE + nux);
            g3[i]      = THREE * g_arr[i];
            gs[i]      = g_arr[i] * shf[i];
            soundsp[i] = std::sqrt(e_arr[i] / (ONE - nux * nux) / rho0[i]);
        }

    } else if (ce != ZERO) {
        // Analytical variable Young's modulus
        for (int i = 0; i < nel; ++i) {
            if (pla[i] > ZERO) {
                e_arr[i]   = e1 - (e1 - einf) * (ONE - std::exp(-ce * pla[i]));
                a1[i]      = e_arr[i] / (ONE - nux * nux);
                a2[i]      = nux * a1[i];
                g_arr[i]   = HALF * e_arr[i] / (ONE + nux);
                g3[i]      = THREE * g_arr[i];
                gs[i]      = g_arr[i] * shf[i];
                soundsp[i] = std::sqrt(e_arr[i] / (ONE - nux * nux) / rho0[i]);
            }
        }
    }

    // ======================================================================
    //  Damage factor (equivalent strain based)
    // ======================================================================
    if (ifail == 2) {
        for (int i = 0; i < nel; ++i) {
            epst[i] = HALF * (epsxx[i] + epsyy[i]
                + std::sqrt((epsxx[i] - epsyy[i]) * (epsxx[i] - epsyy[i])
                            + epsxy[i] * epsxy[i]));
            my_real f = (epsr2 - epst[i]) / (epsr2 - epsr1);
            fail[i] = std::max(EM20, std::min(ONE, f));
            dmg[i]  = ONE - std::max(ZERO, std::min(ONE, f));
        }
    } else {
        for (int i = 0; i < nel; ++i) fail[i] = ONE;
    }

    // ======================================================================
    //  VP == 0  :  total strain-rate dependency
    // ======================================================================
    if (vp == 0) {

        // Remove back-stress
        for (int i = 0; i < nel; ++i) {
            sigoxx[i] -= sigbxx[i];
            sigoyy[i] -= sigbyy[i];
            sigoxy[i] -= sigbxy[i];
        }

        // Elastic trial stress
        for (int i = 0; i < nel; ++i) {
            p0[i]    = -(sigoxx[i] + sigoyy[i]) * THIRD;
            signxx[i] = sigoxx[i] + a1[i]*depsxx[i] + a2[i]*depsyy[i];
            signyy[i] = sigoyy[i] + a2[i]*depsxx[i] + a1[i]*depsyy[i];
            signxy[i] = sigoxy[i] + g_arr[i]*depsxy[i];
            signyz[i] = sigoyz[i] + gs[i]*depsyz[i];
            signzx[i] = sigozx[i] + gs[i]*depszx[i];
            sigexx[i] = signxx[i];
            sigeyy[i] = signyy[i];
            sigexy[i] = signxy[i];
        }

        // Strain rate
        if (israte == 0) {
            for (int i = 0; i < nel; ++i) {
                epsd[i] = HALF * (std::fabs(epspxx[i] + epspyy[i])
                    + std::sqrt((epspxx[i] - epspyy[i]) * (epspxx[i] - epspyy[i])
                                + epspxy[i] * epspxy[i]));
            }
        } else {
            for (int i = 0; i < nel; ++i) {
                epsd[i] = (*asrate) * epsd_pg[i] + (ONE - (*asrate)) * epsd[i];
            }
        }

        // ------------------------------------------------------------------
        //  Pressure-dependent yield factor
        // ------------------------------------------------------------------
        if (pfun > 0) {
            for (int i = 0; i < nel; ++i) {
                pscale[i] = uparam[17 + 2 * nrate - 1] * p0[i];
                iposp[i]  = vartmp_ref(i + 1, 2);
                iadp[i]   = npf[ipfun - 1] / 2 + 1;
                ilenp[i]  = npf[ipfun - 1] / 2 - iadp[i] - iposp[i];
            }
            vinter2_(tf, iadp, iposp, ilenp, nel_p, pscale, dfdp, pfac);
            for (int i = 0; i < nel; ++i) {
                pfac[i] = std::max(ZERO, pfac[i]);
                vartmp_ref(i + 1, 2) = iposp[i];
            }
        } else {
            for (int i = 0; i < nel; ++i) pfac[i] = ONE;
        }

        // ------------------------------------------------------------------
        //  Yield stress and hardening modulus
        // ------------------------------------------------------------------
        if (nrate == 1) {
            // Single (static) curve
            for (int i = 0; i < nel; ++i) {
                ipos1[i] = vartmp_ref(i + 1, 3);
                iad1[i]  = npf[ifunc[1] - 1] / 2 + 1;
                ilen1[i] = npf[ifunc[1]] / 2 - iad1[i] - ipos1[i];
            }
            vinter_(tf, iad1, ipos1, ilen1, nel_p, pla, dydx1, y1);

            for (int i = 0; i < nel; ++i) {
                yfac_ref(i, 0) = uparam[6 + nrate + 1 - 1] * facyldi[i];
                vartmp_ref(i + 1, 3) = ipos1[i];
                fact[i] = fail[i] * pfac[i] * yfac_ref(i, 0);
                h[i]    = dydx1[i] * fact[i];
            }

            if (fisokin == ZERO) {
                for (int i = 0; i < nel; ++i) yld[i] = y1[i] * fact[i];
            } else if (fisokin == ONE) {
                my_real yld0 = tf[npf[ifunc[1] - 1]];   // TF(NPF(IFUNC(1))+1) 1-based
                for (int i = 0; i < nel; ++i) yld[i] = yld0 * fact[i];
            } else if (fisokin > ZERO) {
                my_real yld0 = tf[npf[ifunc[1] - 1]];
                for (int i = 0; i < nel; ++i)
                    yld[i] = ((ONE - fisokin) * y1[i] + fisokin * yld0) * fact[i];
            }

        } else {
            // Multiple strain-rate curves
            for (int i = 0; i < nel; ++i) jj[i] = 1;
            for (int j = 2; j <= nrate - 1; ++j) {
                for (int i = 0; i < nel; ++i) {
                    if (epsd[i] >= uparam[6 + j - 1]) jj[i] = j;
                }
            }

            if (ismooth == 2) {
                // Logarithmic interpolation
                for (int i = 0; i < nel; ++i) {
                    my_real epsp1 = std::max(uparam[6 + jj[i] - 1], EM20);
                    my_real epsp2 = uparam[6 + jj[i] + 1 - 1];
                    rfac[i] = std::log(std::max(epsd[i], EM20) / epsp1)
                            / std::log(epsp2 / epsp1);
                    yfac_ref(i, 0) = uparam[6 + nrate + jj[i] - 1] * facyldi[i];
                    yfac_ref(i, 1) = uparam[7 + nrate + jj[i] - 1] * facyldi[i];
                }
            } else {
                // Linear interpolation
                for (int i = 0; i < nel; ++i) {
                    my_real epsp1 = uparam[6 + jj[i] - 1];
                    my_real epsp2 = uparam[6 + jj[i] + 1 - 1];
                    rfac[i] = (epsd[i] - epsp1) / (epsp2 - epsp1);
                    yfac_ref(i, 0) = uparam[6 + nrate + jj[i] - 1] * facyldi[i];
                    yfac_ref(i, 1) = uparam[7 + nrate + jj[i] - 1] * facyldi[i];
                }
            }

            // Prepare VINTER calls
            for (int i = 0; i < nel; ++i) {
                int j1 = jj[i];
                int j2 = j1 + 1;
                int fun1 = ifunc[j1];
                int fun2 = ifunc[j2];
                ipos1[i] = vartmp_ref(i + 1, 2 + j1);
                ipos2[i] = vartmp_ref(i + 1, 2 + j2);
                iad1[i]  = npf[fun1 - 1] / 2 + 1;
                ilen1[i] = npf[fun1] / 2 - iad1[i] - ipos1[i];
                iad2[i]  = npf[fun2 - 1] / 2 + 1;
                ilen2[i] = npf[fun2] / 2 - iad2[i] - ipos2[i];
            }
            vinter_(tf, iad1, ipos1, ilen1, nel_p, pla, dydx1, y1);
            vinter_(tf, iad2, ipos2, ilen2, nel_p, pla, dydx2, y2);

            if (fisokin == ZERO) {
                for (int i = 0; i < nel; ++i) {
                    my_real fac = rfac[i];
                    y1[i]   *= yfac_ref(i, 0);
                    y2[i]   *= yfac_ref(i, 1);
                    yld[i]   = fail[i] * (y1[i] + fac * (y2[i] - y1[i]));
                    yld[i]   = std::max(yld[i], EM20);
                    dydx1[i] *= yfac_ref(i, 0);
                    dydx2[i] *= yfac_ref(i, 1);
                    h[i]     = fail[i] * (dydx1[i] + fac * (dydx2[i] - dydx1[i]));
                    yld[i]  *= std::max(ZERO, pfac[i]);
                    h[i]    *= std::max(ZERO, pfac[i]);
                }
                for (int i = 0; i < nel; ++i) {
                    vartmp_ref(i + 1, 2 + jj[i])     = ipos1[i];
                    vartmp_ref(i + 1, 2 + jj[i] + 1) = ipos2[i];
                }

            } else if (fisokin == ONE) {
                for (int i = 0; i < nel; ++i) {
                    int fun1 = ifunc[jj[i]];
                    int fun2 = ifunc[jj[i] + 1];
                    my_real fac = rfac[i];
                    dydx1[i] *= yfac_ref(i, 0);
                    dydx2[i] *= yfac_ref(i, 1);
                    h[i]     = fail[i] * (dydx1[i] + fac * (dydx2[i] - dydx1[i]));
                    // Kinematic hardening – use initial yield from table
                    y1[i]    = tf[npf[fun1 - 1]] * yfac_ref(i, 0);
                    y2[i]    = tf[npf[fun2 - 1]] * yfac_ref(i, 1);
                    yld[i]   = fail[i] * (y1[i] + fac * (y2[i] - y1[i]));
                    yld[i]  *= std::max(ZERO, pfac[i]);
                    h[i]    *= std::max(ZERO, pfac[i]);
                }
                for (int i = 0; i < nel; ++i) {
                    vartmp_ref(i + 1, 2 + jj[i])     = ipos1[i];
                    vartmp_ref(i + 1, 2 + jj[i] + 1) = ipos2[i];
                }

            } else {
                // Mixed hardening
                for (int i = 0; i < nel; ++i) {
                    int fun1 = ifunc[jj[i]];
                    int fun2 = ifunc[jj[i] + 1];
                    my_real fac = rfac[i];
                    y1[i]   *= yfac_ref(i, 0);
                    y2[i]   *= yfac_ref(i, 1);
                    my_real yld_iso = fail[i] * (y1[i] + fac * (y2[i] - y1[i]));
                    yld_iso = std::max(yld_iso, EM20);
                    dydx1[i] *= yfac_ref(i, 0);
                    dydx2[i] *= yfac_ref(i, 1);
                    h[i]     = fail[i] * (dydx1[i] + fac * (dydx2[i] - dydx1[i]));
                    // Kinematic part
                    my_real yy1 = tf[npf[fun1 - 1]] * yfac_ref(i, 0);
                    my_real yy2 = tf[npf[fun2 - 1]] * yfac_ref(i, 1);
                    my_real yld_kin = fail[i] * (yy1 + fac * (yy2 - yy1));
                    yld[i] = (ONE - fisokin) * yld_iso + fisokin * yld_kin;
                    yld[i] *= std::max(ZERO, pfac[i]);
                    h[i]   *= std::max(ZERO, pfac[i]);
                }
                for (int i = 0; i < nel; ++i) {
                    vartmp_ref(i + 1, 2 + jj[i])     = ipos1[i];
                    vartmp_ref(i + 1, 2 + jj[i] + 1) = ipos2[i];
                }
            }
        }  // nrate > 1

        if (yldcheck == 1) {
            for (int i = 0; i < nel; ++i) yld[i] = std::max(yld[i], EM20);
        }

        // ------------------------------------------------------------------
        //  Plasticity projection
        // ------------------------------------------------------------------
        if (iplas[0] == 0) {
            // ---- Radial return
            my_real nu3 = ONE - nu_mnu;
            for (int i = 0; i < nel; ++i) {
                svm2[i] = signxx[i]*signxx[i]
                         + signyy[i]*signyy[i]
                         - signxx[i]*signyy[i]
                         + THREE*signxy[i]*signxy[i];
                if (svm2[i] > yld[i] * yld[i]) {
                    my_real svm = std::sqrt(svm2[i]);
                    my_real r   = yld[i] / svm;
                    signxx[i] *= r;
                    signyy[i] *= r;
                    signxy[i] *= r;
                    dpla_i[i]  = off[i] * svm * (ONE - r) / (g3[i] + h[i]);
                    pla[i]    += dpla_i[i];
                    if (inloc == 0) {
                        my_real dezz;
                        if (yld[i] != ZERO) {
                            dezz = dpla_i[i] * HALF * (signxx[i] + signyy[i]) / yld[i];
                        } else {
                            dezz = ZERO;
                        }
                        dezz      = -(depsxx[i] + depsyy[i]) * nu_mnu - nu3 * dezz;
                        thk[i]   += dezz * thkly[i] * off[i];
                    }
                    etse[i] = h[i] / (h[i] + e_arr[i]);
                }
            }

        } else if (iplas[0] == 1) {
            // ---- Implicit Newton (3 iterations)
            for (int i = 0; i < nel; ++i) {
                h[i] = std::max(ZERO, h[i]);
                my_real s1 = signxx[i] + signyy[i];
                my_real s2 = signxx[i] - signyy[i];
                my_real s3 = signxy[i];
                aa[i]   = FOURTH * s1 * s1;
                bb[i]   = THREE_OVER_4 * s2 * s2 + THREE * s3 * s3;
                svm2[i] = aa[i] + bb[i];
            }
            if (inloc == 0) {
                for (int i = 0; i < nel; ++i) {
                    my_real dezz = -(depsxx[i] + depsyy[i]) * nu_mnu;
                    thk[i] += dezz * thkly[i] * off[i];
                }
            }

            // Gather elements in plastic flow
            int nindx = 0;
            for (int i = 0; i < nel; ++i) {
                if (svm2[i] > yld[i] * yld[i] && off[i] == ONE) {
                    index_arr[nindx++] = i;
                }
            }

            if (nindx > 0) {
                for (int jj2 = 0; jj2 < nindx; ++jj2) {
                    int i      = index_arr[jj2];
                    my_real svm = std::sqrt(svm2[i]);
                    dpla_j[i]  = (svm - yld[i]) / (g3[i] + h[i]);
                    etse[i]    = h[i] / (h[i] + e_arr[i]);
                    hi[i]      = h[i] * (ONE - fisokin);
                    hk[i]      = TWO_THIRD * h[i] * fisokin;
                }

                my_real nu3 = ONE - nu_mnu;
                for (int n = 0; n < niter; ++n) {
                    for (int jj2 = 0; jj2 < nindx; ++jj2) {
                        int i = index_arr[jj2];
                        dpla_i[i]      = dpla_j[i];
                        my_real yld_i  = yld[i] + hi[i] * dpla_i[i];
                        dr[i]          = HALF * e_arr[i] * dpla_i[i] / yld_i;
                        my_real aaa    = THREE * hk[i] / e_arr[i];
                        my_real nu11   = u_mnu + aaa;
                        my_real nu21   = t_pnu + aaa;
                        pp[i]          = ONE / (ONE + dr[i] * nu11);
                        qq[i]          = ONE / (ONE + dr[i] * nu21);
                        my_real p2     = pp[i] * pp[i];
                        my_real q2     = qq[i] * qq[i];
                        my_real f_val  = aa[i]*p2 + bb[i]*q2 - yld_i*yld_i;
                        my_real df_val = -(aa[i]*nu11*p2*pp[i] + bb[i]*nu21*q2*qq[i])
                                          * (e_arr[i] - TWO*dr[i]*hi[i]) / yld_i
                                         - TWO * hi[i] * yld_i;
                        // Fortran: DF = SIGN(MAX(ABS(DF),EM20),DF)
                        df_val = std::copysign(
                            std::max(std::fabs(df_val), EM20), df_val);
                        if (dpla_i[i] > ZERO) {
                            dpla_j[i] = std::max(ZERO, dpla_i[i] - f_val / df_val);
                        } else {
                            dpla_j[i] = ZERO;
                        }
                    }
                }

                // Update stresses and plastic strain
                for (int jj2 = 0; jj2 < nindx; ++jj2) {
                    int i = index_arr[jj2];
                    pla[i] += dpla_i[i];
                    my_real s1 = (signxx[i] + signyy[i]) * pp[i];
                    my_real s2 = (signxx[i] - signyy[i]) * qq[i];
                    signxx[i]  = HALF * (s1 + s2);
                    signyy[i]  = HALF * (s1 - s2);
                    signxy[i] *= qq[i];
                    if (inloc == 0) {
                        my_real dezz = -nu3 * dr[i] * s1 / e_arr[i];
                        thk[i] += dezz * thkly[i] * off[i];
                    }
                    yld[i] += hi[i] * dpla_i[i];
                }
            }

        } else if (iplas[0] == 2) {
            // ---- Normal projection + radial return
            for (int i = 0; i < nel; ++i) {
                h[i]    = std::max(ZERO, h[i]);
                svm2[i] = signxx[i]*signxx[i] + signyy[i]*signyy[i]
                         - signxx[i]*signyy[i] + THREE*signxy[i]*signxy[i];
                if (inloc == 0) {
                    my_real dezz = -(depsxx[i] + depsyy[i]) * nu_mnu;
                    thk[i] += dezz * thkly[i] * off[i];
                }
            }

            // Gather
            int nindx = 0;
            for (int i = 0; i < nel; ++i) {
                yld2[i] = yld[i] * yld[i];
                if (svm2[i] > yld2[i] && off[i] == ONE) {
                    index_arr[nindx++] = i;
                }
            }

            if (nindx > 0) {
                my_real nu3 = ONE - nu_mnu;
                for (int jj2 = 0; jj2 < nindx; ++jj2) {
                    int i = index_arr[jj2];
                    my_real a_val = (svm2[i] - yld2[i])
                        / (FIVE*svm2[i]
                           + THREE*(-signxx[i]*signyy[i] + signxy[i]*signxy[i]));
                    my_real s1 = (ONE - TWO*a_val)*signxx[i] + a_val*signyy[i];
                    my_real s2 = a_val*signxx[i] + (ONE - TWO*a_val)*signyy[i];
                    my_real s3 = (ONE - THREE*a_val)*signxy[i];
                    signxx[i]  = s1;
                    signyy[i]  = s2;
                    signxy[i]  = s3;
                    my_real svm = std::sqrt(svm2[i]);
                    dpla_i[i]  = off[i] * (svm - yld[i]) / (g3[i] + h[i]);
                    hk[i]      = h[i] * (ONE - fisokin);
                    yld[i]    += hk[i] * dpla_i[i];
                }

                for (int jj2 = 0; jj2 < nindx; ++jj2) {
                    int i = index_arr[jj2];
                    my_real svm = std::sqrt(signxx[i]*signxx[i]
                                           + signyy[i]*signyy[i]
                                           - signxx[i]*signyy[i]
                                           + THREE*signxy[i]*signxy[i]);
                    my_real r  = std::min(ONE, yld[i] / std::max(EM20, svm));
                    signxx[i] *= r;
                    signyy[i] *= r;
                    signxy[i] *= r;
                    pla[i]    += dpla_i[i];
                    if (inloc == 0) {
                        my_real dezz = dpla_i[i] * HALF * (signxx[i] + signyy[i]) / yld[i];
                        dezz      = -nu3 * dezz;
                        thk[i]   += dezz * thkly[i] * off[i];
                    }
                    etse[i] = h[i] / (h[i] + e_arr[i]);
                }
            }
        }  // iplas

    // ======================================================================
    } else {  // VP == 1  :  plastic strain-rate dependency
    // ======================================================================

        // Remove back-stress
        for (int i = 0; i < nel; ++i) {
            sigoxx[i] -= sigbxx[i];
            sigoyy[i] -= sigbyy[i];
            sigoxy[i] -= sigbxy[i];
        }

        // Elastic trial stress
        for (int i = 0; i < nel; ++i) {
            p0[i]     = -(sigoxx[i] + sigoyy[i]) * THIRD;
            signxx[i] = sigoxx[i] + a1[i]*depsxx[i] + a2[i]*depsyy[i];
            signyy[i] = sigoyy[i] + a2[i]*depsxx[i] + a1[i]*depsyy[i];
            signxy[i] = sigoxy[i] + g_arr[i]*depsxy[i];
            signyz[i] = sigoyz[i] + gs[i]*depsyz[i];
            signzx[i] = sigozx[i] + gs[i]*depszx[i];
            sigexx[i] = signxx[i];
            sigeyy[i] = signyy[i];
            sigexy[i] = signxy[i];
        }

        // Pressure-dependent yield factor
        if (pfun > 0) {
            for (int i = 0; i < nel; ++i) {
                pscale[i] = uparam[17 + 2 * nrate - 1] * p0[i];
                iposp[i]  = vartmp_ref(i + 1, 2);
                iadp[i]   = npf[ipfun - 1] / 2 + 1;
                ilenp[i]  = npf[ipfun - 1] / 2 - iadp[i] - iposp[i];
            }
            vinter2_(tf, iadp, iposp, ilenp, nel_p, pscale, dfdp, pfac);
            for (int i = 0; i < nel; ++i) {
                pfac[i] = std::max(ZERO, pfac[i]);
                vartmp_ref(i + 1, 2) = iposp[i];
            }
        } else {
            for (int i = 0; i < nel; ++i) pfac[i] = ONE;
        }

        // Plastic strain rate from UVAR(:,2)
        for (int i = 0; i < nel; ++i) plap[i] = uvar_ref(i + 1, 2);

        // Bracket element in strain-rate table
        for (int i = 0; i < nel; ++i) jj[i] = 1;
        for (int j = 2; j <= nrate - 1; ++j) {
            for (int i = 0; i < nel; ++i) {
                if (plap[i] >= uparam[6 + j - 1]) jj[i] = j;
            }
        }

        if (ismooth == 2) {
            for (int i = 0; i < nel; ++i) {
                my_real epsp1 = std::max(uparam[6 + jj[i] - 1], EM20);
                my_real epsp2 = uparam[6 + jj[i] + 1 - 1];
                rfac[i] = std::log(std::max(plap[i], EM20) / epsp1)
                        / std::log(epsp2 / epsp1);
            }
        } else {
            for (int i = 0; i < nel; ++i) {
                my_real epsp1 = uparam[6 + jj[i] - 1];
                my_real epsp2 = uparam[6 + jj[i] + 1 - 1];
                rfac[i] = (plap[i] - epsp1) / (epsp2 - epsp1);
            }
        }

        for (int i = 0; i < nel; ++i) {
            yfac_ref(i, 0) = uparam[6 + nrate + jj[i] - 1] * facyldi[i];
            yfac_ref(i, 1) = uparam[7 + nrate + jj[i] - 1] * facyldi[i];
        }

        for (int i = 0; i < nel; ++i) {
            int j1   = jj[i];
            int j2   = j1 + 1;
            int fun1 = ifunc[j1];
            int fun2 = ifunc[j2];
            ipos1[i] = vartmp_ref(i + 1, 2 + j1);
            ipos2[i] = vartmp_ref(i + 1, 2 + j2);
            iad1[i]  = npf[fun1 - 1] / 2 + 1;
            ilen1[i] = npf[fun1] / 2 - iad1[i] - ipos1[i];
            iad2[i]  = npf[fun2 - 1] / 2 + 1;
            ilen2[i] = npf[fun2] / 2 - iad2[i] - ipos2[i];
        }
        vinter_(tf, iad1, ipos1, ilen1, nel_p, pla, dydx1, y1);
        vinter_(tf, iad2, ipos2, ilen2, nel_p, pla, dydx2, y2);

        if (fisokin == ZERO) {
            for (int i = 0; i < nel; ++i) {
                my_real fac = rfac[i];
                y1[i]   *= yfac_ref(i, 0);
                y2[i]   *= yfac_ref(i, 1);
                yld[i]   = fail[i] * (y1[i] + fac * (y2[i] - y1[i]));
                yld[i]   = std::max(yld[i], EM20);
                dydx1[i] *= yfac_ref(i, 0);
                dydx2[i] *= yfac_ref(i, 1);
                h[i]     = fail[i] * (dydx1[i] + fac * (dydx2[i] - dydx1[i]));
                yld[i]  *= std::max(ZERO, pfac[i]);
                h[i]    *= std::max(ZERO, pfac[i]);
            }
            for (int i = 0; i < nel; ++i) {
                vartmp_ref(i + 1, 2 + jj[i])     = ipos1[i];
                vartmp_ref(i + 1, 2 + jj[i] + 1) = ipos2[i];
            }

        } else if (fisokin == ONE) {
            for (int i = 0; i < nel; ++i) {
                int fun1 = ifunc[jj[i]];
                int fun2 = ifunc[jj[i] + 1];
                my_real fac = rfac[i];
                dydx1[i] *= yfac_ref(i, 0);
                dydx2[i] *= yfac_ref(i, 1);
                h[i]     = fail[i] * (dydx1[i] + fac * (dydx2[i] - dydx1[i]));
                y1[i]    = tf[npf[fun1 - 1]] * yfac_ref(i, 0);
                y2[i]    = tf[npf[fun2 - 1]] * yfac_ref(i, 1);
                yld[i]   = fail[i] * (y1[i] + fac * (y2[i] - y1[i]));
                yld[i]  *= std::max(ZERO, pfac[i]);
                h[i]    *= std::max(ZERO, pfac[i]);
            }
            for (int i = 0; i < nel; ++i) {
                vartmp_ref(i + 1, 2 + jj[i])     = ipos1[i];
                vartmp_ref(i + 1, 2 + jj[i] + 1) = ipos2[i];
            }

        } else {
            // Mixed hardening
            for (int i = 0; i < nel; ++i) {
                int fun1 = ifunc[jj[i]];
                int fun2 = ifunc[jj[i] + 1];
                my_real fac = rfac[i];
                y1[i]   *= yfac_ref(i, 0);
                y2[i]   *= yfac_ref(i, 1);
                my_real yld_iso = fail[i] * (y1[i] + fac * (y2[i] - y1[i]));
                yld_iso = std::max(yld_iso, EM20);
                dydx1[i] *= yfac_ref(i, 0);
                dydx2[i] *= yfac_ref(i, 1);
                h[i]     = fail[i] * (dydx1[i] + fac * (dydx2[i] - dydx1[i]));
                my_real yy1 = tf[npf[fun1 - 1]] * yfac_ref(i, 0);
                my_real yy2 = tf[npf[fun2 - 1]] * yfac_ref(i, 1);
                my_real yld_kin = fail[i] * (yy1 + fac * (yy2 - yy1));
                yld[i] = (ONE - fisokin) * yld_iso + fisokin * yld_kin;
                yld[i] *= std::max(ZERO, pfac[i]);
                h[i]   *= std::max(ZERO, pfac[i]);
            }
            for (int i = 0; i < nel; ++i) {
                vartmp_ref(i + 1, 2 + jj[i])     = ipos1[i];
                vartmp_ref(i + 1, 2 + jj[i] + 1) = ipos2[i];
            }
        }

        // Projection (always IPLAS==1 path for VP==1)
        for (int i = 0; i < nel; ++i) {
            h[i] = std::max(ZERO, h[i]);
            my_real s1 = signxx[i] + signyy[i];
            my_real s2 = signxx[i] - signyy[i];
            my_real s3 = signxy[i];
            aa[i]   = FOURTH * s1 * s1;
            bb[i]   = THREE_OVER_4 * s2 * s2 + THREE * s3 * s3;
            svm2[i] = aa[i] + bb[i];
            if (inloc == 0) {
                my_real dezz = -(depsxx[i] + depsyy[i]) * nu_mnu;
                thk[i] += dezz * thkly[i] * off[i];
            }
        }

        // Gather
        int nindx = 0;
        for (int i = 0; i < nel; ++i) {
            if (svm2[i] > yld[i] * yld[i] && off[i] == ONE) {
                index_arr[nindx++] = i;
            }
        }

        if (nindx > 0) {
            for (int jj2 = 0; jj2 < nindx; ++jj2) {
                int i      = index_arr[jj2];
                my_real svm = std::sqrt(svm2[i]);
                dpla_j[i]  = (svm - yld[i]) / (g3[i] + h[i]);
                etse[i]    = h[i] / (h[i] + e_arr[i]);
                hi[i]      = h[i] * (ONE - fisokin);
                hk[i]      = TWO_THIRD * h[i] * fisokin;
            }

            my_real nu3 = ONE - nu_mnu;
            for (int n = 0; n < niter; ++n) {
                for (int jj2 = 0; jj2 < nindx; ++jj2) {
                    int i = index_arr[jj2];
                    dpla_i[i]      = dpla_j[i];
                    my_real yld_i  = yld[i] + hi[i] * dpla_i[i];
                    dr[i]          = HALF * e_arr[i] * dpla_i[i] / yld_i;
                    my_real aaa    = THREE * hk[i] / e_arr[i];
                    my_real nu11   = u_mnu + aaa;
                    my_real nu21   = t_pnu + aaa;
                    pp[i]          = ONE / (ONE + dr[i] * nu11);
                    qq[i]          = ONE / (ONE + dr[i] * nu21);
                    my_real p2     = pp[i] * pp[i];
                    my_real q2     = qq[i] * qq[i];
                    my_real f_val  = aa[i]*p2 + bb[i]*q2 - yld_i*yld_i;
                    my_real df_val = -(aa[i]*nu11*p2*pp[i] + bb[i]*nu21*q2*qq[i])
                                      * (e_arr[i] - TWO*dr[i]*hi[i]) / yld_i
                                     - TWO * hi[i] * yld_i;
                    df_val = std::copysign(
                        std::max(std::fabs(df_val), EM20), df_val);
                    if (dpla_i[i] > ZERO) {
                        dpla_j[i] = std::max(ZERO, dpla_i[i] - f_val / df_val);
                    } else {
                        dpla_j[i] = ZERO;
                    }
                }
            }

            // Apply
            for (int jj2 = 0; jj2 < nindx; ++jj2) {
                int i = index_arr[jj2];
                pla[i] += dpla_i[i];
                my_real s1 = (signxx[i] + signyy[i]) * pp[i];
                my_real s2 = (signxx[i] - signyy[i]) * qq[i];
                signxx[i]  = HALF * (s1 + s2);
                signyy[i]  = HALF * (s1 - s2);
                signxy[i] *= qq[i];
                if (inloc == 0) {
                    my_real dezz = -nu3 * dr[i] * s1 / e_arr[i];
                    thk[i] += dezz * thkly[i] * off[i];
                }
                yld[i] += hi[i] * dpla_i[i];
            }
        }

    }  // VP

    // ======================================================================
    //  Element failure
    // ======================================================================
    if (ifail == 1) {
        if (inloc > 0) {
            for (int i = 0; i < nel; ++i) {
                if (epsmax < EP20) dmg[i] = planl[i] / epsmax;
                if (off[i] == ONE && planl[i] > epsmax) off[i] = FOUR_OVER_5;
            }
        } else {
            for (int i = 0; i < nel; ++i) {
                if (epsmax < EP20) dmg[i] = pla[i] / epsmax;
                if (off[i] == ONE && pla[i] > epsmax) off[i] = FOUR_OVER_5;
            }
        }
    } else if (ifail == 2) {
        if (inloc > 0) {
            for (int i = 0; i < nel; ++i) {
                if (epsmax < EP20) dmg[i] = std::max(dmg[i], planl[i] / epsmax);
                if (off[i] == ONE && (planl[i] > epsmax || epst[i] > epsf))
                    off[i] = FOUR_OVER_5;
            }
        } else {
            for (int i = 0; i < nel; ++i) {
                if (epsmax < EP20) dmg[i] = std::max(dmg[i], pla[i] / epsmax);
                if (off[i] == ONE && (pla[i] > epsmax || epst[i] > epsf))
                    off[i] = FOUR_OVER_5;
            }
        }
    }

    // ======================================================================
    //  Implicit solver output
    // ======================================================================
    if (impl_s > 0) {
        if (ikt > 0) {
            for (int i = 0; i < nel; ++i) {
                if (dpla_i[i] > ZERO) {
                    gama_imp[i]         = THREE_HALF * dpla_i[i] / yld[i];
                    signor_ref(i+1, 4)  = fisokin * h[i];
                    signor_ref(i+1, 5)  = (ONE - fisokin) * h[i];
                    signor_ref(i+1, 1)  = THIRD * (TWO*signxx[i] - signyy[i]);
                    signor_ref(i+1, 2)  = THIRD * (TWO*signyy[i] - signxx[i]);
                    signor_ref(i+1, 3)  = TWO * signxy[i];
                } else {
                    gama_imp[i] = ZERO;
                }
            }
        }
    }

    // ======================================================================
    //  Plastic strain rate filtering  (VP == 1)
    // ======================================================================
    if (vp == 1) {
        my_real dtinv = ONE / std::max(*timestep, EM20);
        for (int i = 0; i < nel; ++i) {
            plap[i]         = (*asrate) * dpla_i[i] * dtinv
                              + (ONE - (*asrate)) * uvar_ref(i + 1, 2);
            uvar_ref(i + 1, 2) = plap[i];
        }
    }

    // ======================================================================
    //  Kinematic hardening  (update back-stress and add to sigma)
    // ======================================================================
    if (fisokin > ZERO) {
        for (int i = 0; i < nel; ++i) {
            my_real hkin  = fisokin * h[i];
            my_real alpha = hkin * dpla_i[i] / yld[i];
            my_real sigpxx = alpha * signxx[i];
            my_real sigpyy = alpha * signyy[i];
            my_real sigpxy = alpha * signxy[i];

            sigbxx[i] += sigpxx;
            sigbyy[i] += sigpyy;
            sigbxy[i] += sigpxy;

            signxx[i] += sigbxx[i];
            signyy[i] += sigbyy[i];
            signxy[i] += sigbxy[i];
        }
    }

    // ======================================================================
    //  Hardening modulus
    // ======================================================================
    for (int i = 0; i < nel; ++i) hardm[i] = h[i];

    // ======================================================================
    //  Non-local thickness variation
    // ======================================================================
    if (inloc > 0) {
        for (int i = 0; i < nel; ++i) {
            if (loff[i] == ONE) {
                my_real svm = std::sqrt(signxx[i]*signxx[i]
                                       + signyy[i]*signyy[i]
                                       - signxx[i]*signyy[i]
                                       + THREE*signxy[i]*signxy[i]);
                my_real dezz = std::max(dplanl[i], ZERO)
                               * HALF * (signxx[i] + signyy[i])
                               / std::max(svm, EM20);
                dezz   = -nux * ((signxx[i] - sigoxx[i] + signyy[i] - sigoyy[i])
                                 / e_arr[i]) - dezz;
                thk[i] += dezz * thkly[i] * off[i];
            }
        }
    }
}
