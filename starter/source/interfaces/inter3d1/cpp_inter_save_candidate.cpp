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
// C++ implementation of INTER_SAVE_CANDIDATE subroutine
// Saves non-zero-penetration candidates into dynamically growing vectors.
// The Fortran version uses MOVE_ALLOC to grow arrays; here std::vector
// handles growth automatically via push_back.
// Callable from Fortran via BIND(C, name="cpp_inter_save_candidate").
#include <vector>

#ifdef MYREAL8
typedef double my_real;
#else
typedef float my_real;
#endif

constexpr int    MVSIZ = 512;
constexpr my_real ZERO = static_cast<my_real>(0.0);

// ======================================================================================================================
// Opaque handle type for the candidate pair vectors.
// Fortran holds a C_PTR to this structure.
// ======================================================================================================================
struct CandidateVectors {
  std::vector<int> cand_n;
  std::vector<int> cand_e;
};

extern "C" {

// ======================================================================================================================
//                                          cpp_candidate_vectors_create
// ======================================================================================================================
// \brief Allocate a new CandidateVectors and return an opaque pointer.
void* cpp_candidate_vectors_create()
{
  return static_cast<void*>(new CandidateVectors());
}

// ======================================================================================================================
//                                          cpp_candidate_vectors_destroy
// ======================================================================================================================
// \brief Destroy a CandidateVectors previously created.
void cpp_candidate_vectors_destroy(void* handle)
{
  delete static_cast<CandidateVectors*>(handle);
}

// ======================================================================================================================
//                                          cpp_candidate_vectors_size
// ======================================================================================================================
// \brief Return the current number of stored candidates.
int cpp_candidate_vectors_size(void* handle)
{
  auto* cv = static_cast<CandidateVectors*>(handle);
  return static_cast<int>(cv->cand_n.size());
}

// ======================================================================================================================
//                                          cpp_candidate_vectors_get
// ======================================================================================================================
// \brief Copy stored candidates into caller-provided arrays.
// \param[out] cand_n_out  array of size >= n_stored
// \param[out] cand_e_out  array of size >= n_stored
// \param[in]  max_size    allocated size of output arrays
void cpp_candidate_vectors_get(void* handle, int* cand_n_out, int* cand_e_out, const int* max_size)
{
  auto* cv = static_cast<CandidateVectors*>(handle);
  int n = static_cast<int>(cv->cand_n.size());
  if (n > *max_size) n = *max_size;
  for (int i = 0; i < n; ++i) {
    cand_n_out[i] = cv->cand_n[i];
    cand_e_out[i] = cv->cand_e[i];
  }
}

// ======================================================================================================================
//                                          cpp_inter_save_candidate
// ======================================================================================================================
//
// \brief Save non-zero-penetration candidate pairs into growing vectors.
// \details For each element i in [0, j_stok), if pene[i] != 0, the pair
//          (prov_n[i], prov_e[i]) is appended to the vectors.
//          std::vector handles reallocation transparently.
//
// \param[in,out] handle    opaque pointer to CandidateVectors
// \param[in]     j_stok    number of provisional candidates in prov_n/prov_e
// \param[in]     prov_n    array of provisional slave node indices (size MVSIZ)
// \param[in]     prov_e    array of provisional segment indices (size MVSIZ)
// \param[in]     pene      penetration values (size MVSIZ)
//
void cpp_inter_save_candidate(
  void*          handle,
  const int*     j_stok_ptr,
  const int*     prov_n,
  const int*     prov_e,
  const my_real* pene)
{
  auto* cv    = static_cast<CandidateVectors*>(handle);
  const int js = *j_stok_ptr;

  for (int i = 0; i < js; ++i) {
    if (pene[i] != ZERO) {
      cv->cand_n.push_back(prov_n[i]);
      cv->cand_e.push_back(prov_e[i]);
    }
  }
}

} // extern "C"
