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
// C++ implementation of I7PEN3 subroutine
// Computes penetration distances and selects the maximum penetration normal.
// Callable from Fortran via BIND(C, name="cpp_i7pen3").
#include <cmath>
#include <algorithm>

#ifdef MYREAL8
typedef double my_real;
#else
typedef float my_real;
#endif

extern "C" {

// ======================================================================================================================
//                                                   cpp_i7pen3
// ======================================================================================================================
//
// \brief Compute penetration distances and select best normal direction
// \details For each candidate (up to last), compute the penetration distance
//          from four edge normals, select the maximum penetration, and normalize
//          the corresponding normal vector.
//
// \param[in]     marge   margin value (scalar, passed by reference)
// \param[in]     gapv    gap values array (size >= last)
// \param[in,out] n1      selected normal x-component (size MVSIZ)
// \param[in,out] n2      selected normal y-component (size MVSIZ)
// \param[in,out] n3      selected normal z-component (size MVSIZ)
// \param[in,out] pene    maximum penetration (size MVSIZ)
// \param[in,out] nx1     normal x from edge 1 (size MVSIZ)
// \param[in,out] ny1     normal y from edge 1 (size MVSIZ)
// \param[in,out] nz1     normal z from edge 1 (size MVSIZ)
// \param[in,out] nx2     normal x from edge 2 (size MVSIZ)
// \param[in,out] ny2     normal y from edge 2 (size MVSIZ)
// \param[in,out] nz2     normal z from edge 2 (size MVSIZ)
// \param[in,out] nx3     normal x from edge 3 (size MVSIZ)
// \param[in,out] ny3     normal y from edge 3 (size MVSIZ)
// \param[in,out] nz3     normal z from edge 3 (size MVSIZ)
// \param[in,out] nx4     normal x from edge 4 (size MVSIZ)
// \param[in,out] ny4     normal y from edge 4 (size MVSIZ)
// \param[in,out] nz4     normal z from edge 4 (size MVSIZ)
// \param[in,out] p1      penetration from edge 1 (size MVSIZ)
// \param[in,out] p2      penetration from edge 2 (size MVSIZ)
// \param[in,out] p3      penetration from edge 3 (size MVSIZ)
// \param[in,out] p4      penetration from edge 4 (size MVSIZ)
// \param[in]     last    number of candidates to process (scalar, passed by reference)
//
void cpp_i7pen3(
  const my_real* marge,
  const my_real* gapv,
  my_real* n1,  my_real* n2,  my_real* n3,
  my_real* pene,
  my_real* nx1, my_real* ny1, my_real* nz1,
  my_real* nx2, my_real* ny2, my_real* nz2,
  my_real* nx3, my_real* ny3, my_real* nz3,
  my_real* nx4, my_real* ny4, my_real* nz4,
  my_real* p1,  my_real* p2,  my_real* p3,  my_real* p4,
  const int* last)
{
  const my_real zero = static_cast<my_real>(0.0);
  const my_real one  = static_cast<my_real>(1.0);
  const my_real em30 = static_cast<my_real>(1.0e-30);

  const my_real margin = *marge;
  const int n = *last;

  // First loop: compute penetration distances from the four edges
  for (int i = 0; i < n; ++i) {
    my_real d1 = std::sqrt(p1[i]);
    p1[i] = std::max(zero, gapv[i] + margin - d1);

    my_real d2 = std::sqrt(p2[i]);
    p2[i] = std::max(zero, gapv[i] + margin - d2);

    my_real d3 = std::sqrt(p3[i]);
    p3[i] = std::max(zero, gapv[i] + margin - d3);

    my_real d4 = std::sqrt(p4[i]);
    p4[i] = std::max(zero, gapv[i] + margin - d4);
  }

  // Second loop: select the maximum penetration and its associated normal
  for (int i = 0; i < n; ++i) {
    pene[i] = std::max({p1[i], p2[i], p3[i], p4[i]});

    if (p1[i] == pene[i]) {
      n1[i] = nx1[i];
      n2[i] = ny1[i];
      n3[i] = nz1[i];
    } else if (p2[i] == pene[i]) {
      n1[i] = nx2[i];
      n2[i] = ny2[i];
      n3[i] = nz2[i];
    } else if (p3[i] == pene[i]) {
      n1[i] = nx3[i];
      n2[i] = ny3[i];
      n3[i] = nz3[i];
    } else if (p4[i] == pene[i]) {
      n1[i] = nx4[i];
      n2[i] = ny4[i];
      n3[i] = nz4[i];
    }
  }

  // Third loop: normalize the selected normal vectors
  for (int i = 0; i < n; ++i) {
    my_real s2 = one / std::max(em30, std::sqrt(n1[i] * n1[i] + n2[i] * n2[i] + n3[i] * n3[i]));
    n1[i] *= s2;
    n2[i] *= s2;
    n3[i] *= s2;
  }
}

} // extern "C"
