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
!||    cpp_i7trivox1_mod   ../starter/source/interfaces/inter3d1/cpp_i7trivox1.F90
!||--- called by ------------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
!! \brief Fortran interface module for the C++ implementation of I7TRIVOX1
!! \details Provides the BIND(C) interface to call cpp_i7trivox1 from Fortran.
!!          Serial version (no OpenMP). The C++ routine manages candidate
!!          storage internally using std::vector and returns the results
!!          via output arrays.
      module cpp_i7trivox1_mod
        use iso_c_binding
        implicit none

#ifdef MYREAL8
        integer, parameter :: c_my_real = c_double
#else
        integer, parameter :: c_my_real = c_float
#endif

        interface
          subroutine cpp_i7trivox1(                                    &
            nsn , irect , x , stf ,                                    &
            stfn , xyzm , nsv ,                                        &
            tzinf , gap_s_l , gap_m_l ,                                &
            voxel , nbx , nby , nbz , nrtm_l ,                        &
            igap , gap , gap_s , gap_m ,                               &
            gapmin , gapmax , marge , curv_max , bgapsmx ,             &
            istf , i_stok ,                                            &
            drad , index ,                                             &
            iremnode , flagremnode , kremnode , remnode ,               &
            dgapload , crvoxel ,                                       &
            iix , iiy , iiz , local_next_nod ,                         &
            nrtm ,                                                     &
            numnod , numfakenodigeo , numels ,                         &
            is_used_with_law151 ,                                      &
            num_cand_out , cand_n_ptr_out ,                            &
            cand_e_ptr_out) bind(C, name="cpp_i7trivox1")
            use iso_c_binding
#ifdef MYREAL8
            integer, parameter :: c_my_real = c_double
#else
            integer, parameter :: c_my_real = c_float
#endif
            integer(c_int),  intent(in)    :: nsn                     !< number of secondary nodes
            integer(c_int),  intent(in)    :: irect(4,*)              !< segment connectivity
            real(c_my_real), intent(in)    :: x(3,*)                  !< global coordinates
            real(c_my_real), intent(in)    :: stf(*)                  !< segment stiffness
            real(c_my_real), intent(in)    :: stfn(*)                 !< node stiffness
            real(c_my_real), intent(in)    :: xyzm(6,2)               !< bounding box data
            integer(c_int),  intent(in)    :: nsv(*)                  !< secondary node indices
            real(c_my_real), intent(in)    :: tzinf                   !< influence zone size
            real(c_my_real), intent(in)    :: gap_s_l(*)              !< secondary gap local
            real(c_my_real), intent(in)    :: gap_m_l(*)              !< master gap local
            integer(c_int),  intent(inout) :: voxel(*)                !< voxel grid
            integer(c_int),  intent(in)    :: nbx                     !< number of voxels in x
            integer(c_int),  intent(in)    :: nby                     !< number of voxels in y
            integer(c_int),  intent(in)    :: nbz                     !< number of voxels in z
            integer(c_int),  intent(in)    :: nrtm_l                  !< number of active segments
            integer(c_int),  intent(in)    :: igap                    !< gap flag
            real(c_my_real), intent(in)    :: gap                     !< global gap
            real(c_my_real), intent(in)    :: gap_s(*)                !< secondary gap
            real(c_my_real), intent(in)    :: gap_m(*)                !< master gap
            real(c_my_real), intent(in)    :: gapmin                  !< minimum gap
            real(c_my_real), intent(in)    :: gapmax                  !< maximum gap
            real(c_my_real), intent(in)    :: marge                   !< margin
            real(c_my_real), intent(in)    :: curv_max(*)             !< max curvature per segment
            real(c_my_real), intent(in)    :: bgapsmx                 !< max secondary gap
            integer(c_int),  intent(in)    :: istf                    !< stiffness flag
            integer(c_int),  intent(inout) :: i_stok                  !< candidate count (unused here)
            real(c_my_real), intent(in)    :: drad                    !< search radius
            integer(c_int),  intent(in)    :: index(*)                !< active segment indices
            integer(c_int),  intent(in)    :: iremnode                !< removed node flag
            integer(c_int),  intent(in)    :: flagremnode             !< removed node mode
            integer(c_int),  intent(in)    :: kremnode(*)             !< removed node offsets
            integer(c_int),  intent(in)    :: remnode(*)              !< removed node list
            real(c_my_real), intent(in)    :: dgapload                !< gap load increment
            integer(c_int),  intent(in)    :: crvoxel(0:31,0:31)     !< coarse voxel bitmap
            integer(c_int),  intent(inout) :: iix(*)                  !< node voxel x index
            integer(c_int),  intent(inout) :: iiy(*)                  !< node voxel y index
            integer(c_int),  intent(inout) :: iiz(*)                  !< node voxel z index
            integer(c_int),  intent(inout) :: local_next_nod(*)       !< linked list next
            integer(c_int),  intent(in)    :: nrtm                    !< total segments
            integer(c_int),  intent(in)    :: numnod                  !< number of nodes
            integer(c_int),  intent(in)    :: numfakenodigeo          !< number of fake IGE nodes
            integer(c_int),  intent(in)    :: numels                  !< number of solid elements
            integer(c_int),  intent(in)    :: is_used_with_law151     !< flag for law 151
            integer(c_int),  intent(out)   :: num_cand_out            !< number of candidates found
            type(c_ptr),     intent(out)   :: cand_n_ptr_out          !< C pointer to malloc'd candidate nodes array
            type(c_ptr),     intent(out)   :: cand_e_ptr_out          !< C pointer to malloc'd candidate segments array
          end subroutine cpp_i7trivox1

          subroutine cpp_i7trivox1_free(ptr) bind(C, name="cpp_i7trivox1_free")
            use iso_c_binding
            type(c_ptr), value, intent(in) :: ptr                     !< C pointer to free
          end subroutine cpp_i7trivox1_free
        end interface

      end module cpp_i7trivox1_mod
