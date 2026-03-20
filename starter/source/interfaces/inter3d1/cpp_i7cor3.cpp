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
// C++ implementation of I7COR3 subroutine
// Gathers slave/master coordinates, computes gap values and stiffness.
// Callable from Fortran via BIND(C, name="cpp_i7cor3").
//
// IMPORTANT: all index arrays (NSV, CAND_E, CAND_N, IRECT, IX1-IX4, NSVG)
// contain Fortran 1-based indices. When used to access C 0-based arrays
// (X, STF, STFN, GAP_S, GAP_M, etc.) we subtract 1.
#include <cmath>
#include <algorithm>

#ifdef MYREAL8
typedef double my_real;
#else
typedef float my_real;
#endif

constexpr int MVSIZ = 512;

extern "C" {

// ======================================================================================================================
//                                                   cpp_i7cor3
// ======================================================================================================================
//
// \brief Gather slave/master data for interface contact candidates
// \details For each candidate pair (up to last):
//          - Gather slave node coordinates from global array X
//          - Compute gap values depending on IGAP flag
//          - Compute stiffness if ISTF != 0
//          - Gather master segment connectivity from IRECT
//          - Gather master node coordinates from X
//
void cpp_i7cor3(
  const my_real* x,
  const int*     irect,
  const int*     nsv,
  const int*     cand_e,
  const int*     cand_n,
  const my_real* stf,
  const my_real* stfn,
  my_real*       gapv,
  const int*     igap_ptr,
  const my_real* gap_ptr,
  const my_real* gap_s,
  const my_real* gap_m,
  const int*     istf_ptr,
  const my_real* gapmin_ptr,
  const my_real* gapmax_ptr,
  const my_real* gap_s_l,
  const my_real* gap_m_l,
  const my_real* drad_ptr,
  int*           ix1,
  int*           ix2,
  int*           ix3,
  int*           ix4,
  int*           nsvg,
  my_real*       x1,
  my_real*       x2,
  my_real*       x3,
  my_real*       x4,
  my_real*       y1,
  my_real*       y2,
  my_real*       y3,
  my_real*       y4,
  my_real*       z1,
  my_real*       z2,
  my_real*       z3,
  my_real*       z4,
  my_real*       xi_out,
  my_real*       yi_out,
  my_real*       zi_out,
  my_real*       stif,
  const my_real* dgapload_ptr,
  const int*     last_ptr)
{
  const int     igap     = *igap_ptr;
  const my_real gap      = *gap_ptr;
  const int     istf     = *istf_ptr;
  const my_real gapmin   = *gapmin_ptr;
  const my_real gapmax   = *gapmax_ptr;
  const my_real drad     = *drad_ptr;
  const my_real dgapload = *dgapload_ptr;
  const int     last     = *last_ptr;

  // ------------------------------------------------------------------------------------------------------------------
  // Gather slave node coordinates
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    // cand_n[i] is a Fortran 1-based index into nsv
    int ig     = nsv[cand_n[i] - 1];    // Fortran 1-based node number
    nsvg[i]    = ig;
    // x is column-major X(3,*): element (k,j) is at x[3*(j-1) + (k-1)]
    xi_out[i]  = x[3 * (ig - 1) + 0];
    yi_out[i]  = x[3 * (ig - 1) + 1];
    zi_out[i]  = x[3 * (ig - 1) + 2];
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Compute gap values
  // ------------------------------------------------------------------------------------------------------------------
  if (igap == 0) {
    for (int i = 0; i < last; ++i) {
      gapv[i] = std::max(gap + dgapload, drad);
    }
  } else {
    for (int i = 0; i < last; ++i) {
      // cand_n[i], cand_e[i] are Fortran 1-based indices
      gapv[i] = gap_s[cand_n[i] - 1] + gap_m[cand_e[i] - 1];
      if (igap == 3) {
        gapv[i] = std::min(gap_s_l[cand_n[i] - 1] + gap_m_l[cand_e[i] - 1], gapv[i]);
      }
      gapv[i] = std::min(gapmax, gapv[i]);
      gapv[i] = std::max(gapmin, gapv[i]);
      gapv[i] = std::max(drad, gapv[i] + dgapload);
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Compute stiffness (optional)
  // ------------------------------------------------------------------------------------------------------------------
  if (istf != 0) {
    for (int i = 0; i < last; ++i) {
      stif[i] = stf[cand_e[i] - 1] * stfn[cand_n[i] - 1];
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Gather master segment connectivity from IRECT(4,*)
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    int l  = cand_e[i];    // Fortran 1-based segment index
    // irect is column-major IRECT(4,*): element (k,j) at irect[4*(j-1) + (k-1)]
    ix1[i] = irect[4 * (l - 1) + 0];
    ix2[i] = irect[4 * (l - 1) + 1];
    ix3[i] = irect[4 * (l - 1) + 2];
    ix4[i] = irect[4 * (l - 1) + 3];
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Gather master node coordinates
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    // ix1-ix4 contain Fortran 1-based node numbers
    int nn;

    nn    = ix1[i];
    x1[i] = x[3 * (nn - 1) + 0];
    y1[i] = x[3 * (nn - 1) + 1];
    z1[i] = x[3 * (nn - 1) + 2];

    nn    = ix2[i];
    x2[i] = x[3 * (nn - 1) + 0];
    y2[i] = x[3 * (nn - 1) + 1];
    z2[i] = x[3 * (nn - 1) + 2];

    nn    = ix3[i];
    x3[i] = x[3 * (nn - 1) + 0];
    y3[i] = x[3 * (nn - 1) + 1];
    z3[i] = x[3 * (nn - 1) + 2];

    nn    = ix4[i];
    x4[i] = x[3 * (nn - 1) + 0];
    y4[i] = x[3 * (nn - 1) + 1];
    z4[i] = x[3 * (nn - 1) + 2];
  }
}

} // extern "C"
