!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    cpp_inter_save_candidate_mod   ../starter/source/interfaces/inter3d1/cpp_inter_save_candidate.F90
!||--- called by ------------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
!! \brief Fortran interface module for C++ candidate vector management
!! \details Provides the BIND(C) interfaces for creating, populating,
!!          querying and destroying the C++ CandidateVectors container.
!!          Uses an opaque C_PTR handle to hold the std::vector pair.
      module cpp_inter_save_candidate_mod
        use iso_c_binding
        implicit none

#ifdef MYREAL8
        integer, parameter :: c_my_real = c_double
#else
        integer, parameter :: c_my_real = c_float
#endif

        interface

          ! Create an empty CandidateVectors, returns opaque C_PTR
          function cpp_candidate_vectors_create() result(handle) bind(C, name="cpp_candidate_vectors_create")
            use iso_c_binding
            type(c_ptr) :: handle
          end function cpp_candidate_vectors_create

          ! Destroy a CandidateVectors
          subroutine cpp_candidate_vectors_destroy(handle) bind(C, name="cpp_candidate_vectors_destroy")
            use iso_c_binding
            type(c_ptr), value :: handle
          end subroutine cpp_candidate_vectors_destroy

          ! Return the number of stored candidates
          function cpp_candidate_vectors_size(handle) result(n) bind(C, name="cpp_candidate_vectors_size")
            use iso_c_binding
            type(c_ptr), value :: handle
            integer(c_int)     :: n
          end function cpp_candidate_vectors_size

          ! Copy stored candidates into Fortran arrays
          subroutine cpp_candidate_vectors_get(handle, cand_n_out, cand_e_out, max_size) &
            bind(C, name="cpp_candidate_vectors_get")
            use iso_c_binding
            type(c_ptr), value          :: handle
            integer(c_int), intent(out) :: cand_n_out(*)              !< output secondary node ids
            integer(c_int), intent(out) :: cand_e_out(*)              !< output segment ids
            integer(c_int), intent(in)  :: max_size                   !< max size of output arrays
          end subroutine cpp_candidate_vectors_get

          ! Save non-zero-penetration candidates from a batch
          subroutine cpp_inter_save_candidate(handle, j_stok, prov_n, prov_e, pene) &
            bind(C, name="cpp_inter_save_candidate")
            use iso_c_binding
#ifdef MYREAL8
            integer, parameter :: c_my_real = c_double
#else
            integer, parameter :: c_my_real = c_float
#endif
            type(c_ptr), value          :: handle
            integer(c_int),  intent(in) :: j_stok                     !< number of provisional candidates
            integer(c_int),  intent(in) :: prov_n(*)                  !< provisional secondary node indices
            integer(c_int),  intent(in) :: prov_e(*)                  !< provisional segment indices
            real(c_my_real), intent(in) :: pene(*)                    !< penetration values
          end subroutine cpp_inter_save_candidate

        end interface

      end module cpp_inter_save_candidate_mod
