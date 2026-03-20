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
!||    cpp_i7dst3_mod   ../starter/source/interfaces/inter3d1/cpp_i7dst3.F90
!||--- called by ------------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
!! \brief Fortran interface module for the C++ implementation of I7DST3
!! \details Provides the BIND(C) interface to call cpp_i7dst3 from Fortran.
!!          The C++ subroutine computes distances from slave nodes to the four
!!          sub-triangles of a master quad segment.
      module cpp_i7dst3_mod
        use iso_c_binding
        implicit none

#ifdef MYREAL8
        integer, parameter :: c_my_real = c_double
#else
        integer, parameter :: c_my_real = c_float
#endif

        interface
          subroutine cpp_i7dst3(ix3 , ix4 , x1 , x2 , x3 , &
            x4 , y1 , y2 , y3 , y4 , &
            z1 , z2 , z3 , z4 , xi , &
            yi , zi , x0 , y0 , z0 , &
            nx1 , ny1 , nz1 , nx2 , ny2 , &
            nz2 , nx3 , ny3 , nz3 , nx4 , &
            ny4 , nz4 , p1 , p2 , p3 , &
            p4 , lb1 , lb2 , lb3 , lb4 , &
            lc1 , lc2 , lc3 , lc4 , last) bind(C, name="cpp_i7dst3")
            use iso_c_binding
#ifdef MYREAL8
            integer, parameter :: c_my_real = c_double
#else
            integer, parameter :: c_my_real = c_float
#endif
            integer(c_int),  intent(in)    :: ix3(*)                  !< node 3 index array
            integer(c_int),  intent(in)    :: ix4(*)                  !< node 4 index array
            real(c_my_real), intent(inout) :: x1(*)                   !< x-coord node 1
            real(c_my_real), intent(inout) :: x2(*)                   !< x-coord node 2
            real(c_my_real), intent(inout) :: x3(*)                   !< x-coord node 3
            real(c_my_real), intent(inout) :: x4(*)                   !< x-coord node 4
            real(c_my_real), intent(inout) :: y1(*)                   !< y-coord node 1
            real(c_my_real), intent(inout) :: y2(*)                   !< y-coord node 2
            real(c_my_real), intent(inout) :: y3(*)                   !< y-coord node 3
            real(c_my_real), intent(inout) :: y4(*)                   !< y-coord node 4
            real(c_my_real), intent(inout) :: z1(*)                   !< z-coord node 1
            real(c_my_real), intent(inout) :: z2(*)                   !< z-coord node 2
            real(c_my_real), intent(inout) :: z3(*)                   !< z-coord node 3
            real(c_my_real), intent(inout) :: z4(*)                   !< z-coord node 4
            real(c_my_real), intent(inout) :: xi(*)                   !< x-coord slave node
            real(c_my_real), intent(inout) :: yi(*)                   !< y-coord slave node
            real(c_my_real), intent(inout) :: zi(*)                   !< z-coord slave node
            real(c_my_real), intent(inout) :: x0(*)                   !< x-coord centroid
            real(c_my_real), intent(inout) :: y0(*)                   !< y-coord centroid
            real(c_my_real), intent(inout) :: z0(*)                   !< z-coord centroid
            real(c_my_real), intent(inout) :: nx1(*)                  !< normal x sub-tri 1
            real(c_my_real), intent(inout) :: ny1(*)                  !< normal y sub-tri 1
            real(c_my_real), intent(inout) :: nz1(*)                  !< normal z sub-tri 1
            real(c_my_real), intent(inout) :: nx2(*)                  !< normal x sub-tri 2
            real(c_my_real), intent(inout) :: ny2(*)                  !< normal y sub-tri 2
            real(c_my_real), intent(inout) :: nz2(*)                  !< normal z sub-tri 2
            real(c_my_real), intent(inout) :: nx3(*)                  !< normal x sub-tri 3
            real(c_my_real), intent(inout) :: ny3(*)                  !< normal y sub-tri 3
            real(c_my_real), intent(inout) :: nz3(*)                  !< normal z sub-tri 3
            real(c_my_real), intent(inout) :: nx4(*)                  !< normal x sub-tri 4
            real(c_my_real), intent(inout) :: ny4(*)                  !< normal y sub-tri 4
            real(c_my_real), intent(inout) :: nz4(*)                  !< normal z sub-tri 4
            real(c_my_real), intent(inout) :: p1(*)                   !< squared dist sub-tri 1
            real(c_my_real), intent(inout) :: p2(*)                   !< squared dist sub-tri 2
            real(c_my_real), intent(inout) :: p3(*)                   !< squared dist sub-tri 3
            real(c_my_real), intent(inout) :: p4(*)                   !< squared dist sub-tri 4
            real(c_my_real), intent(inout) :: lb1(*)                  !< bary coord b sub-tri 1
            real(c_my_real), intent(inout) :: lb2(*)                  !< bary coord b sub-tri 2
            real(c_my_real), intent(inout) :: lb3(*)                  !< bary coord b sub-tri 3
            real(c_my_real), intent(inout) :: lb4(*)                  !< bary coord b sub-tri 4
            real(c_my_real), intent(inout) :: lc1(*)                  !< bary coord c sub-tri 1
            real(c_my_real), intent(inout) :: lc2(*)                  !< bary coord c sub-tri 2
            real(c_my_real), intent(inout) :: lc3(*)                  !< bary coord c sub-tri 3
            real(c_my_real), intent(inout) :: lc4(*)                  !< bary coord c sub-tri 4
            integer(c_int),  intent(in)    :: last                    !< number of candidates
          end subroutine cpp_i7dst3
        end interface

      end module cpp_i7dst3_mod
