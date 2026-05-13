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
!||    get_list_remnode_cpp_mod   ../starter/source/interfaces/inter3d1/get_list_remnode_cpp.F90
!||--- called by ------------------------------------------------------
!||    i7remnode                  ../starter/source/interfaces/inter3d1/i7remnode.F
!||====================================================================
      module get_list_remnode_cpp_mod
        use iso_c_binding
        implicit none

        ! C++ interface: Dijkstra-based BFS for remnode computation
        interface
          subroutine c_remnode_bfs(nrtm, numnod, igap, irect, knod2seg, nod2seg, &
            tagsecnd, x, gap_m, gap_m_l, gapsecnd, gap_s_l_tmp,               &
            gapmin, gapmax, gap, drad, dgapload, gaps_mx, gaps_l_mx, minseg,   &
            kremnode_counts, remnode_out, remnode_size) bind(C, name="c_remnode_bfs_")
            import :: c_int, c_double, c_float, c_ptr
            integer(c_int), intent(in) :: nrtm
            integer(c_int), intent(in) :: numnod
            integer(c_int), intent(in) :: igap
            integer(c_int), intent(in) :: irect(*)
            integer(c_int), intent(in) :: knod2seg(*)
            integer(c_int), intent(in) :: nod2seg(*)
            integer(c_int), intent(in) :: tagsecnd(*)
#ifndef MYREAL4
            real(c_double), intent(in) :: x(*)
            real(c_double), intent(in) :: gap_m(*)
            real(c_double), intent(in) :: gap_m_l(*)
            real(c_double), intent(in) :: gapsecnd(*)
            real(c_double), intent(in) :: gap_s_l_tmp(*)
            real(c_double), intent(in) :: gapmin
            real(c_double), intent(in) :: gapmax
            real(c_double), intent(in) :: gap
            real(c_double), intent(in) :: drad
            real(c_double), intent(in) :: dgapload
            real(c_double), intent(in) :: gaps_mx
            real(c_double), intent(in) :: gaps_l_mx
            real(c_double), intent(in) :: minseg
#else
            real(c_float), intent(in) :: x(*)
            real(c_float), intent(in) :: gap_m(*)
            real(c_float), intent(in) :: gap_m_l(*)
            real(c_float), intent(in) :: gapsecnd(*)
            real(c_float), intent(in) :: gap_s_l_tmp(*)
            real(c_float), intent(in) :: gapmin
            real(c_float), intent(in) :: gapmax
            real(c_float), intent(in) :: gap
            real(c_float), intent(in) :: drad
            real(c_float), intent(in) :: dgapload
            real(c_float), intent(in) :: gaps_mx
            real(c_float), intent(in) :: gaps_l_mx
            real(c_float), intent(in) :: minseg
#endif
            integer(c_int), intent(out) :: kremnode_counts(*)
            type(c_ptr), intent(out) :: remnode_out
            integer(c_int), intent(out) :: remnode_size
          end subroutine c_remnode_bfs

          subroutine c_remnode_bfs_free(remnode_out) bind(C, name="c_remnode_bfs_free_")
            import :: c_ptr
            type(c_ptr), intent(inout) :: remnode_out
          end subroutine c_remnode_bfs_free
        end interface

      contains

! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief C++-accelerated version of get_list_remnode using Dijkstra algorithm.
!! \details Calls the C++ Dijkstra BFS, then handles prefix sum, buffer resize
!!          (upgrade_remnode), and copy into intbuf_tab%remnode on the Fortran side.
!!          This routine replaces the OpenMP-parallel Fortran BFS version.
!!          It is NOT called from within an OMP PARALLEL region.
        subroutine get_list_remnode_cpp(nrtm, igap, numnod, npari, irect, kremnode,  &
          knod2seg, nod2seg, tagsecnd,                                              &
          ipari, gapmin, gapmax, gap, drad,                                         &
          gaps_mx, gaps_l_mx,                                                       &
          minseg, dgapload, x, gap_m,                                               &
          gap_m_l, gapsecnd_arr, gap_s_l_tmp,                                       &
          intbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use intbufdef_mod , only : intbuf_struct_
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nrtm
          integer, intent(in) :: igap
          integer, intent(in) :: numnod
          integer, intent(in) :: npari
          integer, dimension(4,nrtm), intent(in) :: irect
          integer, dimension(nrtm+1), intent(inout) :: kremnode
          integer, dimension(numnod+1), intent(in) :: knod2seg
          integer, dimension(4*nrtm), intent(in) :: nod2seg
          integer, dimension(numnod), intent(in) :: tagsecnd
          integer, dimension(npari), intent(in) :: ipari
          real(kind=WP), intent(in) :: gapmin
          real(kind=WP), intent(in) :: gapmax
          real(kind=WP), intent(in) :: gap
          real(kind=WP), intent(in) :: drad
          real(kind=WP), intent(in) :: gaps_mx
          real(kind=WP), intent(in) :: gaps_l_mx
          real(kind=WP), intent(in) :: minseg
          real(kind=WP), intent(in) :: dgapload
          real(kind=WP), dimension(3,numnod), intent(in) :: x
          real(kind=WP), dimension(nrtm), intent(in) :: gap_m
          real(kind=WP), dimension(nrtm), intent(in) :: gap_m_l
          real(kind=WP), dimension(numnod), intent(in) :: gapsecnd_arr
          real(kind=WP), dimension(numnod), intent(in) :: gap_s_l_tmp
          type(intbuf_struct_), intent(inout) :: intbuf_tab
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, nty, my_size, total_remnode
          integer(c_int) :: c_nrtm, c_numnod, c_igap, c_remnode_size
          integer(c_int), allocatable :: kremnode_counts(:)
          type(c_ptr) :: c_remnode_ptr
          integer(c_int), pointer :: remnode_flat(:)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
          ! Allocate per-segment count array
          allocate(kremnode_counts(nrtm))

          ! Convert integers for C interface
          c_nrtm   = int(nrtm, c_int)
          c_numnod = int(numnod, c_int)
          c_igap   = int(igap, c_int)

          ! Call C++ Dijkstra BFS
          call c_remnode_bfs(c_nrtm, c_numnod, c_igap,   &
            irect, knod2seg, nod2seg, tagsecnd, x,      &
            gap_m, gap_m_l, gapsecnd_arr, gap_s_l_tmp,  &
            gapmin, gapmax, gap, drad, dgapload,        &
            gaps_mx, gaps_l_mx, minseg,                 &
            kremnode_counts, c_remnode_ptr, c_remnode_size)

          total_remnode = int(c_remnode_size)

          ! Build kremnode CSR index (prefix sum)
          kremnode(1) = 0
          do i = 1, nrtm
            kremnode(i+1) = kremnode(i) + int(kremnode_counts(i))
          end do

          ! Resize intbuf_tab%remnode if needed
          my_size = intbuf_tab%s_remnode
          nty = ipari(7)
          if (kremnode(nrtm+1) > my_size) then
            call upgrade_remnode(ipari, kremnode(nrtm+1), intbuf_tab, nty)
          end if

          ! Copy results from C array into Fortran intbuf_tab%remnode
          if (total_remnode > 0) then
            call c_f_pointer(c_remnode_ptr, remnode_flat, [total_remnode])
            do i = 1, total_remnode
              intbuf_tab%remnode(i) = int(remnode_flat(i))
            end do
          end if

          ! Free C++ allocated memory
          call c_remnode_bfs_free(c_remnode_ptr)

          ! Cleanup
          deallocate(kremnode_counts)

          return
        end subroutine get_list_remnode_cpp

      end module get_list_remnode_cpp_mod
