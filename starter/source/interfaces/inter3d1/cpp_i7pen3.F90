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
!||    cpp_i7pen3_mod   ../starter/source/interfaces/inter3d1/cpp_i7pen3.F90
!||--- called by ------------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
!! \brief Fortran interface module for the C++ implementation of I7PEN3
!! \details Provides the BIND(C) interface to call cpp_i7pen3 from Fortran.
!!          The C++ subroutine computes penetration distances from four edges,
!!          selects the maximum penetration, and normalizes the resulting normal.
      module cpp_i7pen3_mod
        use iso_c_binding
        implicit none

#ifdef MYREAL8
        integer, parameter :: c_my_real = c_double
#else
        integer, parameter :: c_my_real = c_float
#endif

        interface
          subroutine cpp_i7pen3(marge , gapv , n1 , n2 , n3 , &
            pene , nx1 , ny1 , nz1 , nx2 , &
            ny2 , nz2 , nx3 , ny3 , nz3 , &
            nx4 , ny4 , nz4 , p1 , p2 , &
            p3 , p4 , last) bind(C, name="cpp_i7pen3")
            use iso_c_binding
#ifdef MYREAL8
            integer, parameter :: c_my_real = c_double
#else
            integer, parameter :: c_my_real = c_float
#endif
            real(c_my_real), intent(in)    :: marge                   !< margin value
            real(c_my_real), intent(in)    :: gapv(*)                 !< gap values array
            real(c_my_real), intent(inout) :: n1(*)                   !< selected normal x-component
            real(c_my_real), intent(inout) :: n2(*)                   !< selected normal y-component
            real(c_my_real), intent(inout) :: n3(*)                   !< selected normal z-component
            real(c_my_real), intent(inout) :: pene(*)                 !< maximum penetration
            real(c_my_real), intent(inout) :: nx1(*)                  !< normal x from edge 1
            real(c_my_real), intent(inout) :: ny1(*)                  !< normal y from edge 1
            real(c_my_real), intent(inout) :: nz1(*)                  !< normal z from edge 1
            real(c_my_real), intent(inout) :: nx2(*)                  !< normal x from edge 2
            real(c_my_real), intent(inout) :: ny2(*)                  !< normal y from edge 2
            real(c_my_real), intent(inout) :: nz2(*)                  !< normal z from edge 2
            real(c_my_real), intent(inout) :: nx3(*)                  !< normal x from edge 3
            real(c_my_real), intent(inout) :: ny3(*)                  !< normal y from edge 3
            real(c_my_real), intent(inout) :: nz3(*)                  !< normal z from edge 3
            real(c_my_real), intent(inout) :: nx4(*)                  !< normal x from edge 4
            real(c_my_real), intent(inout) :: ny4(*)                  !< normal y from edge 4
            real(c_my_real), intent(inout) :: nz4(*)                  !< normal z from edge 4
            real(c_my_real), intent(inout) :: p1(*)                   !< penetration from edge 1
            real(c_my_real), intent(inout) :: p2(*)                   !< penetration from edge 2
            real(c_my_real), intent(inout) :: p3(*)                   !< penetration from edge 3
            real(c_my_real), intent(inout) :: p4(*)                   !< penetration from edge 4
            integer(c_int),  intent(in)    :: last                    !< number of candidates
          end subroutine cpp_i7pen3
        end interface

      end module cpp_i7pen3_mod
