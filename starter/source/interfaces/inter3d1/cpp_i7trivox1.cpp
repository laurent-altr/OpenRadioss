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
// C++ implementation of I7TRIVOX1 subroutine (serial version, no OpenMP).
// Classifies secondary nodes into voxels, searches for segment/node candidate
// pairs, and stores them into growing vectors.
// Callable from Fortran via BIND(C, name="cpp_i7trivox1").
#include <cmath>
#include <algorithm>
#include <vector>
#include <numeric>
#include <cstring>

#ifdef MYREAL8
typedef double my_real;
#else
typedef float my_real;
#endif

// ----------------------------------------------------------------------------------------------------------------------
//                                                   CONSTANTS
// ----------------------------------------------------------------------------------------------------------------------
constexpr int     MVSIZ   = 512;
constexpr int     LRVOXEL = 31;
constexpr my_real ZERO    = static_cast<my_real>(0.0);
constexpr my_real ONE     = static_cast<my_real>(1.0);

// Forward declarations of the sub-routines (defined in other cpp files)
extern "C" {
void cpp_i7cor3(
  const my_real* x, const int* irect, const int* nsv,
  const int* cand_e, const int* cand_n,
  const my_real* stf, const my_real* stfn,
  my_real* gapv, const int* igap, const my_real* gap,
  const my_real* gap_s, const my_real* gap_m,
  const int* istf, const my_real* gapmin, const my_real* gapmax,
  const my_real* gap_s_l, const my_real* gap_m_l, const my_real* drad,
  int* ix1, int* ix2, int* ix3, int* ix4, int* nsvg,
  my_real* x1, my_real* x2, my_real* x3, my_real* x4,
  my_real* y1, my_real* y2, my_real* y3, my_real* y4,
  my_real* z1, my_real* z2, my_real* z3, my_real* z4,
  my_real* xi, my_real* yi, my_real* zi,
  my_real* stif, const my_real* dgapload, const int* last);

void cpp_i7dst3(
  const int* ix3, const int* ix4,
  my_real* x1, my_real* x2, my_real* x3, my_real* x4,
  my_real* y1, my_real* y2, my_real* y3, my_real* y4,
  my_real* z1, my_real* z2, my_real* z3, my_real* z4,
  my_real* xi, my_real* yi, my_real* zi,
  my_real* x0, my_real* y0, my_real* z0,
  my_real* nx1, my_real* ny1, my_real* nz1,
  my_real* nx2, my_real* ny2, my_real* nz2,
  my_real* nx3, my_real* ny3, my_real* nz3,
  my_real* nx4, my_real* ny4, my_real* nz4,
  my_real* p1, my_real* p2, my_real* p3, my_real* p4,
  my_real* lb1, my_real* lb2, my_real* lb3, my_real* lb4,
  my_real* lc1, my_real* lc2, my_real* lc3, my_real* lc4,
  const int* last);

void cpp_i7pen3(
  const my_real* marge, const my_real* gapv,
  my_real* n1, my_real* n2, my_real* n3,
  my_real* pene,
  my_real* nx1, my_real* ny1, my_real* nz1,
  my_real* nx2, my_real* ny2, my_real* nz2,
  my_real* nx3, my_real* ny3, my_real* nz3,
  my_real* nx4, my_real* ny4, my_real* nz4,
  my_real* p1, my_real* p2, my_real* p3, my_real* p4,
  const int* last);
} // extern "C" forward declarations

// Internal helper: process a batch of j_stok candidates through cor3/dst3/pen3
// and save non-zero penetration pairs.
static void process_batch(
  int j_stok,
  const my_real* x, const int* irect, const int* nsv,
  int* prov_e, int* prov_n,
  const my_real* stf, const my_real* stfn,
  my_real* gapv,
  const int* igap, const my_real* gap,
  const my_real* gap_s, const my_real* gap_m,
  const int* istf, const my_real* gapmin, const my_real* gapmax,
  const my_real* gap_s_l, const my_real* gap_m_l, const my_real* drad,
  const my_real* marge, const my_real* dgapload,
  // scratch arrays (all MVSIZ)
  int*     ix11,  int*     ix12,  int*     ix13,  int*     ix14, int* nsvg,
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
  my_real* n11, my_real* n21, my_real* n31,
  my_real* pene, my_real* stif,
  // output candidate vectors
  std::vector<int>& cand_n_vec,
  std::vector<int>& cand_e_vec)
{
  cpp_i7cor3(x, irect, nsv, prov_e, prov_n,
             stf, stfn, gapv, igap, gap,
             gap_s, gap_m, istf, gapmin, gapmax,
             gap_s_l, gap_m_l, drad,
             ix11, ix12, ix13, ix14, nsvg,
             x1, x2, x3, x4,
             y1, y2, y3, y4,
             z1, z2, z3, z4,
             xi, yi, zi, stif, dgapload, &j_stok);

  cpp_i7dst3(ix13, ix14, x1, x2, x3, x4,
             y1, y2, y3, y4,
             z1, z2, z3, z4,
             xi, yi, zi, x0, y0, z0,
             nx1, ny1, nz1, nx2, ny2, nz2,
             nx3, ny3, nz3, nx4, ny4, nz4,
             p1, p2, p3, p4,
             lb1, lb2, lb3, lb4,
             lc1, lc2, lc3, lc4, &j_stok);

  cpp_i7pen3(marge, gapv, n11, n21, n31,
             pene, nx1, ny1, nz1, nx2, ny2, nz2,
             nx3, ny3, nz3, nx4, ny4, nz4,
             p1, p2, p3, p4, &j_stok);

  // Save non-zero penetration candidates
  for (int i = 0; i < j_stok; ++i) {
    if (pene[i] != ZERO) {
      cand_n_vec.push_back(prov_n[i]);
      cand_e_vec.push_back(prov_e[i]);
    }
  }
}

extern "C" {

// ======================================================================================================================
//                                                   cpp_i7trivox1
// ======================================================================================================================
//
// \brief Voxel-based contact search: classify nodes, find segment/node pairs
// \details Serial (no OpenMP) version. The algorithm:
//   1. Place slave nodes into voxel grid cells
//   2. For each master segment, find overlapping voxels
//   3. For each slave node in those voxels, run proximity tests
//   4. Accumulate candidate pairs into a batch; when full, process via
//      I7COR3 -> I7DST3 -> I7PEN3, then save non-zero-penetration pairs
//   5. Copy final candidate pairs into output arrays
//
// Parameters matching the Fortran signature, all passed by reference.
// Arrays use Fortran 1-based indexing conventions.
//
// \param[out] num_cand_out  number of candidates found (added to i_stok)
// \param[out] cand_n_out    output slave node indices (size >= num_cand_out)
// \param[out] cand_e_out    output segment indices (size >= num_cand_out)
// \param[in]  cand_out_maxsize  max allocated size of cand_n_out/cand_e_out
//
void cpp_i7trivox1(
  const int*     nsn_ptr,
  const int*     irect,        // (4, nrtm), column-major
  const my_real* x,            // (3, *), column-major
  const my_real* stf,
  const my_real* stfn,
  const my_real* xyzm,         // (6, 2), column-major
  const int*     nsv,
  const my_real* tzinf_ptr,
  const my_real* gap_s_l,
  const my_real* gap_m_l,
  int*           voxel,        // (nbx+2, nby+2, nbz+2), column-major
  const int*     nbx_ptr,
  const int*     nby_ptr,
  const int*     nbz_ptr,
  const int*     nrtm_l_ptr,
  const int*     igap_ptr,
  const my_real* gap_ptr,
  const my_real* gap_s,
  const my_real* gap_m,
  const my_real* gapmin_ptr,
  const my_real* gapmax_ptr,
  const my_real* marge_ptr,
  const my_real* curv_max,
  const my_real* bgapsmx_ptr,
  const int*     istf_ptr,
  int*           i_stok_ptr,
  const my_real* drad_ptr,
  const int*     index,        // index of active segments
  const int*     iremnode_ptr,
  const int*     flagremnode_ptr,
  const int*     kremnode,
  const int*     remnode,
  const my_real* dgapload_ptr,
  const int*     crvoxel,      // (0:LRVOXEL, 0:LRVOXEL), row-major in Fortran
  int*           iix,          // (nsn)
  int*           iiy,          // (nsn)
  int*           iiz,          // (nsn)
  int*           local_next_nod, // (nsn)
  const int*     nrtm_ptr,
  const int*     numnod_ptr,
  const int*     numfakenodigeo_ptr,
  const int*     numels_ptr,
  const int*     is_used_with_law151_ptr,
  // output
  int*           num_cand_out,
  int*           cand_n_out,
  int*           cand_e_out,
  const int*     cand_out_maxsize_ptr)
{
  // Unpack scalar arguments
  const int     nsn            = *nsn_ptr;
  const my_real tzinf          = *tzinf_ptr;
  const int     nbx            = *nbx_ptr;
  const int     nby            = *nby_ptr;
  const int     nbz            = *nbz_ptr;
  const int     nrtm_l         = *nrtm_l_ptr;
  const int     igap           = *igap_ptr;
  const my_real gap            = *gap_ptr;
  const my_real gapmin         = *gapmin_ptr;
  const my_real gapmax         = *gapmax_ptr;
  const my_real marge          = *marge_ptr;
  const my_real bgapsmx        = *bgapsmx_ptr;
  const int     istf           = *istf_ptr;
  const my_real drad           = *drad_ptr;
  const int     iremnode       = *iremnode_ptr;
  const int     flagremnode    = *flagremnode_ptr;
  const my_real dgapload       = *dgapload_ptr;
  const int     nrtm           = *nrtm_ptr;
  const int     numnod         = *numnod_ptr;
  const int     numfakenodigeo = *numfakenodigeo_ptr;
  const int     numels         = *numels_ptr;
  const int     is_used_151    = *is_used_with_law151_ptr;
  const int     cand_out_maxsize = *cand_out_maxsize_ptr;

  // Voxel grid dimensions (Fortran: VOXEL(NBX+2, NBY+2, NBZ+2))
  const int vdim_x = nbx + 2;
  const int vdim_y = nby + 2;
  const int vdim_z = nbz + 2;

  // Macro: voxel access (convert Fortran 1-based (ix,iy,iz) to 0-based flat index)
  // Fortran column-major: VOXEL(ix,iy,iz) -> voxel[(iz-1)*vdim_y*vdim_x + (iy-1)*vdim_x + (ix-1)]
  #define VOXEL_IDX(ix,iy,iz) (((iz)-1)*vdim_y*vdim_x + ((iy)-1)*vdim_x + ((ix)-1))

  // XYZM(6,2) column-major: element (i,j) at xyzm[6*(j-1) + (i-1)]
  // Column 1: global bounds, Column 2: reduced box bounds
  const my_real xmin  = xyzm[6*0 + 0];
  const my_real ymin  = xyzm[6*0 + 1];
  const my_real zmin  = xyzm[6*0 + 2];
  const my_real xmax  = xyzm[6*0 + 3];
  const my_real ymax  = xyzm[6*0 + 4];
  const my_real zmax  = xyzm[6*0 + 5];
  const my_real xminb = xyzm[6*1 + 0];
  const my_real yminb = xyzm[6*1 + 1];
  const my_real zminb = xyzm[6*1 + 2];
  const my_real xmaxb = xyzm[6*1 + 3];
  const my_real ymaxb = xyzm[6*1 + 4];
  const my_real zmaxb = xyzm[6*1 + 5];

  // CRVOXEL(0:LRVOXEL, 0:LRVOXEL) column-major
  // Element (iy, iz) at crvoxel[iz*(LRVOXEL+1) + iy]
  const int crdim = LRVOXEL + 1;
  #define CRVOXEL_VAL(iy,iz) crvoxel[(iz)*crdim + (iy)]

  // Helper: Fortran BTEST(i, pos) — test bit 'pos' in integer 'i'
  auto btest = [](int val, int pos) -> bool {
    return (val >> pos) & 1;
  };

  // LAST_NOD: temporary, used during node placement
  std::vector<int> last_nod(nsn + 1, 0); // 1-based, index [1..nsn]

  // ==================================================================================================================
  // 1. Place slave nodes into voxel grid
  // ==================================================================================================================
  for (int i = 0; i < nsn; ++i) {
    // i is 0-based, Fortran I is 1-based
    iix[i] = 0;
    iiy[i] = 0;
    iiz[i] = 0;
    if (stfn[i] == ZERO) continue;
    int j = nsv[i]; // Fortran 1-based node id

    // First test vs CRVOXEL
    int iv_x = static_cast<int>(LRVOXEL * (x[3*(j-1)+0] - xmin) / (xmax - xmin));
    if (iv_x < 0 || iv_x > LRVOXEL) continue;
    int iv_y = static_cast<int>(LRVOXEL * (x[3*(j-1)+1] - ymin) / (ymax - ymin));
    if (iv_y < 0 || iv_y > LRVOXEL) continue;
    int iv_z = static_cast<int>(LRVOXEL * (x[3*(j-1)+2] - zmin) / (zmax - zmin));
    if (iv_z < 0 || iv_z > LRVOXEL) continue;
    if (!btest(CRVOXEL_VAL(iv_y, iv_z), iv_x)) continue;

    // Second test vs reduced box — compute voxel indices
    int vix, viy, viz;

    if ((x[3*(j-1)+0] - xminb) / (xmaxb - xminb) > ONE)
      vix = nbx;
    else
      vix = static_cast<int>(std::max(nbx * (x[3*(j-1)+0] - xminb) / (xmaxb - xminb), -ONE));

    if ((x[3*(j-1)+1] - yminb) / (ymaxb - yminb) > ONE)
      viy = nby;
    else
      viy = static_cast<int>(std::max(nby * (x[3*(j-1)+1] - yminb) / (ymaxb - yminb), -ONE));

    if ((x[3*(j-1)+2] - zminb) / (zmaxb - zminb) > ONE)
      viz = nbz;
    else
      viz = static_cast<int>(std::max(nbz * (x[3*(j-1)+2] - zminb) / (zmaxb - zminb), -ONE));

    vix = std::max(1, 2 + vix);
    viy = std::max(1, 2 + viy);
    viz = std::max(1, 2 + viz);

    // Store for later cleanup (1-based)
    iix[i] = vix;
    iiy[i] = viy;
    iiz[i] = viz;

    int fi = i + 1; // Fortran 1-based index of this slave node entry

    int first = voxel[VOXEL_IDX(vix, viy, viz)];
    if (first == 0) {
      // Empty cell
      voxel[VOXEL_IDX(vix, viy, viz)] = fi;
      local_next_nod[i] = 0;
      last_nod[fi] = 0;
    } else if (last_nod[first] == 0) {
      // Cell with one node — add as next
      local_next_nod[first - 1] = fi;
      last_nod[first] = fi;
      local_next_nod[i] = 0;
    } else {
      // Jump to the last node and append
      int last = last_nod[first];
      local_next_nod[last - 1] = fi;
      last_nod[first] = fi;
      local_next_nod[i] = 0;
    }
  }

  // ==================================================================================================================
  // 2. Allocate TAGNOD for node exclusion
  // ==================================================================================================================
  int stagnod = numnod + numfakenodigeo;
  if (is_used_151) stagnod += numels;
  std::vector<int> tagnod(stagnod, 0);

  // ==================================================================================================================
  // 3. Search voxels for each master segment and create candidate pairs
  // ==================================================================================================================
  // Scratch arrays of size MVSIZ
  int     prov_n[MVSIZ], prov_e[MVSIZ];
  my_real gapv[MVSIZ];
  int     ix11[MVSIZ], ix12[MVSIZ], ix13[MVSIZ], ix14[MVSIZ], nsvg[MVSIZ];
  my_real arr_x1[MVSIZ], arr_x2[MVSIZ], arr_x3[MVSIZ], arr_x4[MVSIZ];
  my_real arr_y1[MVSIZ], arr_y2[MVSIZ], arr_y3[MVSIZ], arr_y4[MVSIZ];
  my_real arr_z1[MVSIZ], arr_z2[MVSIZ], arr_z3[MVSIZ], arr_z4[MVSIZ];
  my_real arr_xi[MVSIZ], arr_yi[MVSIZ], arr_zi[MVSIZ];
  my_real arr_x0[MVSIZ], arr_y0[MVSIZ], arr_z0[MVSIZ];
  my_real arr_nx1[MVSIZ], arr_ny1[MVSIZ], arr_nz1[MVSIZ];
  my_real arr_nx2[MVSIZ], arr_ny2[MVSIZ], arr_nz2[MVSIZ];
  my_real arr_nx3[MVSIZ], arr_ny3[MVSIZ], arr_nz3[MVSIZ];
  my_real arr_nx4[MVSIZ], arr_ny4[MVSIZ], arr_nz4[MVSIZ];
  my_real arr_p1[MVSIZ], arr_p2[MVSIZ], arr_p3[MVSIZ], arr_p4[MVSIZ];
  my_real arr_lb1[MVSIZ], arr_lb2[MVSIZ], arr_lb3[MVSIZ], arr_lb4[MVSIZ];
  my_real arr_lc1[MVSIZ], arr_lc2[MVSIZ], arr_lc3[MVSIZ], arr_lc4[MVSIZ];
  my_real arr_n11[MVSIZ], arr_n21[MVSIZ], arr_n31[MVSIZ];
  my_real arr_stif[MVSIZ], arr_pene[MVSIZ];

  // Dynamic candidate storage
  std::vector<int> cand_n_vec;
  std::vector<int> cand_e_vec;
  cand_n_vec.reserve(1024);
  cand_e_vec.reserve(1024);

  int j_stok = 0;

  for (int kk = 0; kk < nrtm_l; ++kk) {
    int ne = index[kk]; // Fortran 1-based segment index

    if (stf[ne - 1] == ZERO) continue;

    // Tag removed nodes
    if (flagremnode == 2 && iremnode == 2) {
      int k_start = kremnode[ne - 1]; // kremnode is 0-based offset here
      int k_end   = kremnode[ne];     // (Fortran: KREMNODE(NE)+1 to KREMNODE(NE+1))
      for (int m = k_start; m < k_end; ++m) {
        tagnod[remnode[m] - 1] = 1;
      }
    }

    // Compute search distance AAA
    my_real aaa;
    if (igap == 0) {
      aaa = tzinf + curv_max[ne - 1];
    } else {
      aaa = marge + curv_max[ne - 1]
          + std::max(std::min(gapmax, std::max(gapmin, bgapsmx + gap_m[ne - 1])) + dgapload, drad);
    }

    // Gather master segment node coordinates
    // IRECT(4,*) column-major: element (k,j) at irect[4*(j-1)+(k-1)]
    int m1 = irect[4*(ne-1) + 0];
    int m2 = irect[4*(ne-1) + 1];
    int m3 = irect[4*(ne-1) + 2];
    int m4 = irect[4*(ne-1) + 3];

    my_real xx1 = x[3*(m1-1) + 0];
    my_real xx2 = x[3*(m2-1) + 0];
    my_real xx3 = x[3*(m3-1) + 0];
    my_real xx4 = x[3*(m4-1) + 0];
    my_real xmaxe = std::max({xx1, xx2, xx3, xx4});
    my_real xmine = std::min({xx1, xx2, xx3, xx4});

    my_real yy1 = x[3*(m1-1) + 1];
    my_real yy2 = x[3*(m2-1) + 1];
    my_real yy3 = x[3*(m3-1) + 1];
    my_real yy4 = x[3*(m4-1) + 1];
    my_real ymaxe = std::max({yy1, yy2, yy3, yy4});
    my_real ymine = std::min({yy1, yy2, yy3, yy4});

    my_real zz1 = x[3*(m1-1) + 2];
    my_real zz2 = x[3*(m2-1) + 2];
    my_real zz3 = x[3*(m3-1) + 2];
    my_real zz4 = x[3*(m4-1) + 2];
    my_real zmaxe = std::max({zz1, zz2, zz3, zz4});
    my_real zmine = std::min({zz1, zz2, zz3, zz4});

    // Surface normal (approximate) for candidate elimination
    my_real sx = (yy3 - yy1) * (zz4 - zz2) - (zz3 - zz1) * (yy4 - yy2);
    my_real sy = (zz3 - zz1) * (xx4 - xx2) - (xx3 - xx1) * (zz4 - zz2);
    my_real sz = (xx3 - xx1) * (yy4 - yy2) - (yy3 - yy1) * (xx4 - xx2);
    my_real s2 = sx * sx + sy * sy + sz * sz;

    // Compute voxel index range for the segment
    auto voxel_lo = [](my_real coord_min, my_real aaa_val, my_real box_min, my_real box_max, int nb) -> int {
      if ((coord_min - aaa_val - box_min) / (box_max - box_min) > ONE) return nb;
      return static_cast<int>(std::max(nb * (coord_min - aaa_val - box_min) / (box_max - box_min), -ONE));
    };
    auto voxel_hi = [](my_real coord_max, my_real aaa_val, my_real box_min, my_real box_max, int nb) -> int {
      if ((coord_max + aaa_val - box_min) / (box_max - box_min) > ONE) return nb;
      return static_cast<int>(std::max(nb * (coord_max + aaa_val - box_min) / (box_max - box_min), -ONE));
    };

    int ix1 = std::max(1, 2 + voxel_lo(xmine, aaa, xminb, xmaxb, nbx));
    int iy1 = std::max(1, 2 + voxel_lo(ymine, aaa, yminb, ymaxb, nby));
    int iz1 = std::max(1, 2 + voxel_lo(zmine, aaa, zminb, zmaxb, nbz));
    int ix2 = std::max(1, 2 + voxel_hi(xmaxe, aaa, xminb, xmaxb, nbx));
    int iy2 = std::max(1, 2 + voxel_hi(ymaxe, aaa, yminb, ymaxb, nby));
    int iz2 = std::max(1, 2 + voxel_hi(zmaxe, aaa, zminb, zmaxb, nbz));

    // Search overlapping voxels
    for (int iz = iz1; iz <= iz2; ++iz) {
      for (int iy = iy1; iy <= iy2; ++iy) {
        for (int ix = ix1; ix <= ix2; ++ix) {

          // Walk the linked list of slave nodes in this voxel cell
          for (int jj = voxel[VOXEL_IDX(ix, iy, iz)]; jj != 0; jj = local_next_nod[jj - 1]) {
            // jj is 1-based index into nsv, stfn, etc.
            int nn = nsv[jj - 1]; // Fortran 1-based global node number

            // Skip tagged / self-contact nodes
            if (tagnod[nn - 1] == 1 || nn == m1 || nn == m2 || nn == m3 || nn == m4) continue;

            my_real xs = x[3*(nn-1) + 0];
            my_real ys = x[3*(nn-1) + 1];
            my_real zs = x[3*(nn-1) + 2];

            my_real local_aaa = aaa;
            if (igap != 0) {
              local_aaa = marge + curv_max[ne - 1]
                  + std::max(std::min(gapmax, std::max(gapmin, gap_s[jj - 1] + gap_m[ne - 1])) + dgapload, drad);
            }

            // Bounding box test
            if (xs <= xmine - local_aaa || xs >= xmaxe + local_aaa ||
                ys <= ymine - local_aaa || ys >= ymaxe + local_aaa ||
                zs <= zmine - local_aaa || zs >= zmaxe + local_aaa) continue;

            // Distance underestimation test for candidate elimination
            my_real d1x = xs - xx1;
            my_real d1y = ys - yy1;
            my_real d1z = zs - zz1;
            my_real d2x = xs - xx2;
            my_real d2y = ys - yy2;
            my_real d2z = zs - zz2;
            my_real dd1 = d1x * sx + d1y * sy + d1z * sz;
            my_real dd2 = d2x * sx + d2y * sy + d2z * sz;
            if (dd1 * dd2 > ZERO) {
              my_real d2_val = std::min(dd1 * dd1, dd2 * dd2);
              my_real a2_val = local_aaa * local_aaa * s2;
              if (d2_val > a2_val) continue;
            }

            // Accept candidate
            prov_n[j_stok] = jj;
            prov_e[j_stok] = ne;
            j_stok++;

            if (j_stok == MVSIZ) {
              process_batch(j_stok, x, irect, nsv, prov_e, prov_n,
                            stf, stfn, gapv, &igap, &gap,
                            gap_s, gap_m, &istf, &gapmin, &gapmax,
                            gap_s_l, gap_m_l, &drad, &marge, &dgapload,
                            ix11, ix12, ix13, ix14, nsvg,
                            arr_x1, arr_x2, arr_x3, arr_x4,
                            arr_y1, arr_y2, arr_y3, arr_y4,
                            arr_z1, arr_z2, arr_z3, arr_z4,
                            arr_xi, arr_yi, arr_zi,
                            arr_x0, arr_y0, arr_z0,
                            arr_nx1, arr_ny1, arr_nz1,
                            arr_nx2, arr_ny2, arr_nz2,
                            arr_nx3, arr_ny3, arr_nz3,
                            arr_nx4, arr_ny4, arr_nz4,
                            arr_p1, arr_p2, arr_p3, arr_p4,
                            arr_lb1, arr_lb2, arr_lb3, arr_lb4,
                            arr_lc1, arr_lc2, arr_lc3, arr_lc4,
                            arr_n11, arr_n21, arr_n31,
                            arr_pene, arr_stif,
                            cand_n_vec, cand_e_vec);
              j_stok = 0;
            }
          } // for jj (linked list walk)

        }
      }
    }

    // Untag removed nodes
    if (flagremnode == 2 && iremnode == 2) {
      int k_start = kremnode[ne - 1];
      int k_end   = kremnode[ne];
      for (int m = k_start; m < k_end; ++m) {
        tagnod[remnode[m] - 1] = 0;
      }
    }

  } // for kk (segments)

  // Process remaining candidates
  if (j_stok != 0) {
    process_batch(j_stok, x, irect, nsv, prov_e, prov_n,
                  stf, stfn, gapv, &igap, &gap,
                  gap_s, gap_m, &istf, &gapmin, &gapmax,
                  gap_s_l, gap_m_l, &drad, &marge, &dgapload,
                  ix11, ix12, ix13, ix14, nsvg,
                  arr_x1, arr_x2, arr_x3, arr_x4,
                  arr_y1, arr_y2, arr_y3, arr_y4,
                  arr_z1, arr_z2, arr_z3, arr_z4,
                  arr_xi, arr_yi, arr_zi,
                  arr_x0, arr_y0, arr_z0,
                  arr_nx1, arr_ny1, arr_nz1,
                  arr_nx2, arr_ny2, arr_nz2,
                  arr_nx3, arr_ny3, arr_nz3,
                  arr_nx4, arr_ny4, arr_nz4,
                  arr_p1, arr_p2, arr_p3, arr_p4,
                  arr_lb1, arr_lb2, arr_lb3, arr_lb4,
                  arr_lc1, arr_lc2, arr_lc3, arr_lc4,
                  arr_n11, arr_n21, arr_n31,
                  arr_pene, arr_stif,
                  cand_n_vec, cand_e_vec);
    j_stok = 0;
  }

  // ==================================================================================================================
  // 4. Sort candidates for reproducibility (sort by (cand_n, cand_e) pairs)
  // ==================================================================================================================
  {
    int n_cand = static_cast<int>(cand_n_vec.size());
    if (n_cand > 0) {
      std::vector<int> idx(n_cand);
      std::iota(idx.begin(), idx.end(), 0);
      std::sort(idx.begin(), idx.end(), [&](int a, int b) {
        if (cand_n_vec[a] != cand_n_vec[b]) return cand_n_vec[a] < cand_n_vec[b];
        return cand_e_vec[a] < cand_e_vec[b];
      });

      // Apply permutation
      std::vector<int> sorted_n(n_cand), sorted_e(n_cand);
      for (int i = 0; i < n_cand; ++i) {
        sorted_n[i] = cand_n_vec[idx[i]];
        sorted_e[i] = cand_e_vec[idx[i]];
      }
      cand_n_vec.swap(sorted_n);
      cand_e_vec.swap(sorted_e);
    }
  }

  // ==================================================================================================================
  // 5. Copy candidates to output arrays
  // ==================================================================================================================
  int n_cand = static_cast<int>(cand_n_vec.size());
  int n_copy = std::min(n_cand, cand_out_maxsize);
  for (int i = 0; i < n_copy; ++i) {
    cand_n_out[i] = cand_n_vec[i];
    cand_e_out[i] = cand_e_vec[i];
  }
  *num_cand_out = n_cand;

  // ==================================================================================================================
  // 6. Cleanup voxel grid
  // ==================================================================================================================
  for (int i = 0; i < nsn; ++i) {
    if (iix[i] != 0) {
      voxel[VOXEL_IDX(iix[i], iiy[i], iiz[i])] = 0;
    }
  }

  #undef VOXEL_IDX
  #undef CRVOXEL_VAL
}

} // extern "C"
