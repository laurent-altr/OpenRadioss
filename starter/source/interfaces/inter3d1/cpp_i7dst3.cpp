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
// C++ implementation of I7DST3 subroutine
// Computes distances from slave nodes to master segments (quad sub-triangles).
// Callable from Fortran via BIND(C, name="cpp_i7dst3").
#include <cmath>
#include <algorithm>

#ifdef MYREAL8
typedef double my_real;
#else
typedef float my_real;
#endif

// ----------------------------------------------------------------------------------------------------------------------
//                                                   CONSTANTS
// ----------------------------------------------------------------------------------------------------------------------
constexpr int    MVSIZ   = 512;
constexpr my_real ZERO   = static_cast<my_real>(0.0);
constexpr my_real ONE    = static_cast<my_real>(1.0);
constexpr my_real FOURTH = static_cast<my_real>(0.25);
constexpr my_real EM20   = static_cast<my_real>(1.0e-20);
constexpr my_real EM30   = static_cast<my_real>(1.0e-30);

extern "C" {

// ======================================================================================================================
//                                                   cpp_i7dst3
// ======================================================================================================================
//
// \brief Compute distances from slave nodes to four sub-triangles of a master segment
// \details For each candidate (up to last), decompose the quad into four triangles
//          sharing the centroid, compute barycentric coordinates, project onto the
//          closest point in each triangle, and return the squared distance and
//          normal direction for each sub-triangle.
//
void cpp_i7dst3(
  const int*    ix3,  const int*    ix4,
  my_real* x1,  my_real* x2,  my_real* x3,  my_real* x4,
  my_real* y1,  my_real* y2,  my_real* y3,  my_real* y4,
  my_real* z1,  my_real* z2,  my_real* z3,  my_real* z4,
  my_real* xi,  my_real* yi,  my_real* zi,
  my_real* x0,  my_real* y0,  my_real* z0,
  my_real* nx1, my_real* ny1, my_real* nz1,
  my_real* nx2, my_real* ny2, my_real* nz2,
  my_real* nx3, my_real* ny3, my_real* nz3,
  my_real* nx4, my_real* ny4, my_real* nz4,
  my_real* p1,  my_real* p2,  my_real* p3,  my_real* p4,
  my_real* lb1, my_real* lb2, my_real* lb3, my_real* lb4,
  my_real* lc1, my_real* lc2, my_real* lc3, my_real* lc4,
  const int* last_ptr)
{
  const int last = *last_ptr;

  // Local arrays
  my_real al1[MVSIZ], al2[MVSIZ], al3[MVSIZ], al4[MVSIZ];
  my_real x01[MVSIZ], x02[MVSIZ], x03[MVSIZ], x04[MVSIZ];
  my_real y01[MVSIZ], y02[MVSIZ], y03[MVSIZ], y04[MVSIZ];
  my_real z01[MVSIZ], z02[MVSIZ], z03[MVSIZ], z04[MVSIZ];
  my_real xi1[MVSIZ], xi2[MVSIZ], xi3[MVSIZ], xi4[MVSIZ];
  my_real yi1[MVSIZ], yi2[MVSIZ], yi3[MVSIZ], yi4[MVSIZ];
  my_real zi1[MVSIZ], zi2[MVSIZ], zi3[MVSIZ], zi4[MVSIZ];
  my_real hlb1[MVSIZ], hlc1[MVSIZ], hlb2[MVSIZ], hlc2[MVSIZ];
  my_real hlb3[MVSIZ], hlc3[MVSIZ], hlb4[MVSIZ], hlc4[MVSIZ];

  // ------------------------------------------------------------------------------------------------------------------
  // Compute centroid X0, Y0, Z0
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    x0[i] = FOURTH * (x1[i] + x2[i] + x3[i] + x4[i]);
    y0[i] = FOURTH * (y1[i] + y2[i] + y3[i] + y4[i]);
    z0[i] = FOURTH * (z1[i] + z2[i] + z3[i] + z4[i]);
  }

  // Degenerate quad (triangle): set centroid to node 3
  // Fortran uses 1-based IX3/IX4, comparison still valid
  for (int i = 0; i < last; ++i) {
    if (ix3[i] == ix4[i]) {
      x0[i] = x3[i];
      y0[i] = y3[i];
      z0[i] = z3[i];
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Compute relative vectors and barycentric coordinates for four sub-triangles
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    // Vectors from centroid to corners
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

    // Vector from centroid to slave node
    my_real xi0 = x0[i] - xi[i];
    my_real yi0 = y0[i] - yi[i];
    my_real zi0 = z0[i] - zi[i];

    // Vectors from corners to slave node
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

    // Cross products for sub-triangle 1 (centroid, node1, node2)
    my_real sx1 = yi0 * zi1[i] - zi0 * yi1[i];
    my_real sy1 = zi0 * xi1[i] - xi0 * zi1[i];
    my_real sz1 = xi0 * yi1[i] - yi0 * xi1[i];

    my_real sx2 = yi0 * zi2[i] - zi0 * yi2[i];
    my_real sy2 = zi0 * xi2[i] - xi0 * zi2[i];
    my_real sz2 = xi0 * yi2[i] - yi0 * xi2[i];

    my_real sx0 = y01[i] * z02[i] - z01[i] * y02[i];
    my_real sy0 = z01[i] * x02[i] - x01[i] * z02[i];
    my_real sz0 = x01[i] * y02[i] - y01[i] * x02[i];
    my_real s2  = ONE / std::max(EM30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);

    lb1[i] = -(sx0 * sx2 + sy0 * sy2 + sz0 * sz2) * s2;
    lc1[i] =  (sx0 * sx1 + sy0 * sy1 + sz0 * sz1) * s2;

    // Cross products for sub-triangle 2 (centroid, node2, node3)
    my_real sx3 = yi0 * zi3[i] - zi0 * yi3[i];
    my_real sy3 = zi0 * xi3[i] - xi0 * zi3[i];
    my_real sz3 = xi0 * yi3[i] - yi0 * xi3[i];

    sx0 = y02[i] * z03[i] - z02[i] * y03[i];
    sy0 = z02[i] * x03[i] - x02[i] * z03[i];
    sz0 = x02[i] * y03[i] - y02[i] * x03[i];
    s2  = ONE / std::max(EM30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);

    lb2[i] = -(sx0 * sx3 + sy0 * sy3 + sz0 * sz3) * s2;
    lc2[i] =  (sx0 * sx2 + sy0 * sy2 + sz0 * sz2) * s2;

    // Cross products for sub-triangle 3 (centroid, node3, node4)
    my_real sx4 = yi0 * zi4[i] - zi0 * yi4[i];
    my_real sy4 = zi0 * xi4[i] - xi0 * zi4[i];
    my_real sz4 = xi0 * yi4[i] - yi0 * xi4[i];

    sx0 = y03[i] * z04[i] - z03[i] * y04[i];
    sy0 = z03[i] * x04[i] - x03[i] * z04[i];
    sz0 = x03[i] * y04[i] - y03[i] * x04[i];
    s2  = ONE / std::max(EM30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);

    lb3[i] = -(sx0 * sx4 + sy0 * sy4 + sz0 * sz4) * s2;
    lc3[i] =  (sx0 * sx3 + sy0 * sy3 + sz0 * sz3) * s2;

    // Cross products for sub-triangle 4 (centroid, node4, node1)
    sx0 = y04[i] * z01[i] - z04[i] * y01[i];
    sy0 = z04[i] * x01[i] - x04[i] * z01[i];
    sz0 = x04[i] * y01[i] - y04[i] * x01[i];
    s2  = ONE / std::max(EM30, sx0 * sx0 + sy0 * sy0 + sz0 * sz0);

    lb4[i] = -(sx0 * sx1 + sy0 * sy1 + sz0 * sz1) * s2;
    lc4[i] =  (sx0 * sx4 + sy0 * sy4 + sz0 * sz4) * s2;

    // Compute edge-based corrections for sharp angle handling
    my_real aaa;

    aaa       = ONE / std::max(EM30, x01[i] * x01[i] + y01[i] * y01[i] + z01[i] * z01[i]);
    hlc1[i]   = lc1[i] * std::abs(lc1[i]) * aaa;
    hlb4[i]   = lb4[i] * std::abs(lb4[i]) * aaa;
    al1[i]    = -(xi0 * x01[i] + yi0 * y01[i] + zi0 * z01[i]) * aaa;
    al1[i]    = std::max(ZERO, std::min(ONE, al1[i]));

    aaa       = ONE / std::max(EM30, x02[i] * x02[i] + y02[i] * y02[i] + z02[i] * z02[i]);
    hlc2[i]   = lc2[i] * std::abs(lc2[i]) * aaa;
    hlb1[i]   = lb1[i] * std::abs(lb1[i]) * aaa;
    al2[i]    = -(xi0 * x02[i] + yi0 * y02[i] + zi0 * z02[i]) * aaa;
    al2[i]    = std::max(ZERO, std::min(ONE, al2[i]));

    aaa       = ONE / std::max(EM30, x03[i] * x03[i] + y03[i] * y03[i] + z03[i] * z03[i]);
    hlc3[i]   = lc3[i] * std::abs(lc3[i]) * aaa;
    hlb2[i]   = lb2[i] * std::abs(lb2[i]) * aaa;
    al3[i]    = -(xi0 * x03[i] + yi0 * y03[i] + zi0 * z03[i]) * aaa;
    al3[i]    = std::max(ZERO, std::min(ONE, al3[i]));

    aaa       = ONE / std::max(EM30, x04[i] * x04[i] + y04[i] * y04[i] + z04[i] * z04[i]);
    hlc4[i]   = lc4[i] * std::abs(lc4[i]) * aaa;
    hlb3[i]   = lb3[i] * std::abs(lb3[i]) * aaa;
    al4[i]    = -(xi0 * x04[i] + yi0 * y04[i] + zi0 * z04[i]) * aaa;
    al4[i]    = std::max(ZERO, std::min(ONE, al4[i]));
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Correction loops for triangle sharp angles — edge 1-2
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    my_real x12 = x2[i] - x1[i];
    my_real y12 = y2[i] - y1[i];
    my_real z12 = z2[i] - z1[i];
    my_real la  = ONE - lb1[i] - lc1[i];
    my_real aaa = ONE / std::max(EM20, x12 * x12 + y12 * y12 + z12 * z12);
    my_real hla = la * std::abs(la) * aaa;

    if (la < ZERO && hla <= hlb1[i] && hla <= hlc1[i]) {
      lb1[i] = (xi2[i] * x12 + yi2[i] * y12 + zi2[i] * z12) * aaa;
      lb1[i] = std::max(ZERO, std::min(ONE, lb1[i]));
      lc1[i] = ONE - lb1[i];
    } else if (lb1[i] < ZERO && hlb1[i] <= hlc1[i] && hlb1[i] <= hla) {
      lb1[i] = ZERO;
      lc1[i] = al2[i];
    } else if (lc1[i] < ZERO && hlc1[i] <= hla && hlc1[i] <= hlb1[i]) {
      lc1[i] = ZERO;
      lb1[i] = al1[i];
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Correction loops for triangle sharp angles — edge 2-3
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    my_real x23 = x3[i] - x2[i];
    my_real y23 = y3[i] - y2[i];
    my_real z23 = z3[i] - z2[i];
    my_real la  = ONE - lb2[i] - lc2[i];
    my_real aaa = ONE / std::max(EM20, x23 * x23 + y23 * y23 + z23 * z23);
    my_real hla = la * std::abs(la) * aaa;

    if (la < ZERO && hla <= hlb2[i] && hla <= hlc2[i]) {
      lb2[i] = (xi3[i] * x23 + yi3[i] * y23 + zi3[i] * z23) * aaa;
      lb2[i] = std::max(ZERO, std::min(ONE, lb2[i]));
      lc2[i] = ONE - lb2[i];
    } else if (lb2[i] < ZERO && hlb2[i] <= hlc2[i] && hlb2[i] <= hla) {
      lb2[i] = ZERO;
      lc2[i] = al3[i];
    } else if (lc2[i] < ZERO && hlc2[i] <= hla && hlc2[i] <= hlb2[i]) {
      lc2[i] = ZERO;
      lb2[i] = al2[i];
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Correction loops for triangle sharp angles — edge 3-4
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    my_real x34 = x4[i] - x3[i];
    my_real y34 = y4[i] - y3[i];
    my_real z34 = z4[i] - z3[i];
    my_real la  = ONE - lb3[i] - lc3[i];
    my_real aaa = ONE / std::max(EM20, x34 * x34 + y34 * y34 + z34 * z34);
    my_real hla = la * std::abs(la) * aaa;

    if (la < ZERO && hla <= hlb3[i] && hla <= hlc3[i]) {
      lb3[i] = (xi4[i] * x34 + yi4[i] * y34 + zi4[i] * z34) * aaa;
      lb3[i] = std::max(ZERO, std::min(ONE, lb3[i]));
      lc3[i] = ONE - lb3[i];
    } else if (lb3[i] < ZERO && hlb3[i] <= hlc3[i] && hlb3[i] <= hla) {
      lb3[i] = ZERO;
      lc3[i] = al4[i];
    } else if (lc3[i] < ZERO && hlc3[i] <= hla && hlc3[i] <= hlb3[i]) {
      lc3[i] = ZERO;
      lb3[i] = al3[i];
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Correction loops for triangle sharp angles — edge 4-1
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    my_real x41 = x1[i] - x4[i];
    my_real y41 = y1[i] - y4[i];
    my_real z41 = z1[i] - z4[i];
    my_real la  = ONE - lb4[i] - lc4[i];
    my_real aaa = ONE / std::max(EM20, x41 * x41 + y41 * y41 + z41 * z41);
    my_real hla = la * std::abs(la) * aaa;

    if (la < ZERO && hla <= hlb4[i] && hla <= hlc4[i]) {
      lb4[i] = (xi1[i] * x41 + yi1[i] * y41 + zi1[i] * z41) * aaa;
      lb4[i] = std::max(ZERO, std::min(ONE, lb4[i]));
      lc4[i] = ONE - lb4[i];
    } else if (lb4[i] < ZERO && hlb4[i] <= hlc4[i] && hlb4[i] <= hla) {
      lb4[i] = ZERO;
      lc4[i] = al1[i];
    } else if (lc4[i] < ZERO && hlc4[i] <= hla && hlc4[i] <= hlb4[i]) {
      lc4[i] = ZERO;
      lb4[i] = al4[i];
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Compute normal vectors and squared distances for each sub-triangle
  // ------------------------------------------------------------------------------------------------------------------
  for (int i = 0; i < last; ++i) {
    // Sub-triangle 1 (centroid, node1, node2)
    nx1[i] = xi[i] - (x0[i] + lb1[i] * x01[i] + lc1[i] * x02[i]);
    ny1[i] = yi[i] - (y0[i] + lb1[i] * y01[i] + lc1[i] * y02[i]);
    nz1[i] = zi[i] - (z0[i] + lb1[i] * z01[i] + lc1[i] * z02[i]);
    p1[i]  = nx1[i] * nx1[i] + ny1[i] * ny1[i] + nz1[i] * nz1[i];

    // Sub-triangle 2 (centroid, node2, node3)
    nx2[i] = xi[i] - (x0[i] + lb2[i] * x02[i] + lc2[i] * x03[i]);
    ny2[i] = yi[i] - (y0[i] + lb2[i] * y02[i] + lc2[i] * y03[i]);
    nz2[i] = zi[i] - (z0[i] + lb2[i] * z02[i] + lc2[i] * z03[i]);
    p2[i]  = nx2[i] * nx2[i] + ny2[i] * ny2[i] + nz2[i] * nz2[i];

    // Sub-triangle 3 (centroid, node3, node4)
    nx3[i] = xi[i] - (x0[i] + lb3[i] * x03[i] + lc3[i] * x04[i]);
    ny3[i] = yi[i] - (y0[i] + lb3[i] * y03[i] + lc3[i] * y04[i]);
    nz3[i] = zi[i] - (z0[i] + lb3[i] * z03[i] + lc3[i] * z04[i]);
    p3[i]  = nx3[i] * nx3[i] + ny3[i] * ny3[i] + nz3[i] * nz3[i];

    // Sub-triangle 4 (centroid, node4, node1)
    nx4[i] = xi[i] - (x0[i] + lb4[i] * x04[i] + lc4[i] * x01[i]);
    ny4[i] = yi[i] - (y0[i] + lb4[i] * y04[i] + lc4[i] * y01[i]);
    nz4[i] = zi[i] - (z0[i] + lb4[i] * z04[i] + lc4[i] * z01[i]);
    p4[i]  = nx4[i] * nx4[i] + ny4[i] * ny4[i] + nz4[i] * nz4[i];
  }
}

} // extern "C"
