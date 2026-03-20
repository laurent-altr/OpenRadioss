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
!||    cpp_i7cor3_mod   ../starter/source/interfaces/inter3d1/cpp_i7cor3.F90
!||--- called by ------------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
!! \brief Fortran interface module for the C++ implementation of I7COR3
!! \details Provides the BIND(C) interface to call cpp_i7cor3 from Fortran.
!!          The C++ subroutine gathers slave/master coordinates, computes
!!          gap values and stiffness for interface contact candidates.
      module cpp_i7cor3_mod
        use iso_c_binding
        implicit none

#ifdef MYREAL8
        integer, parameter :: c_my_real = c_double
#else
        integer, parameter :: c_my_real = c_float
#endif

        interface
          subroutine cpp_i7cor3(x , irect , nsv , cand_e , cand_n ,    &
            stf , stfn , gapv , igap , gap ,       &
            gap_s , gap_m , istf , gapmin , gapmax,&
            gap_s_l , gap_m_l , drad ,             &
            ix1 , ix2 , ix3 , ix4 , nsvg ,        &
            x1 , x2 , x3 , x4 ,                   &
            y1 , y2 , y3 , y4 ,                   &
            z1 , z2 , z3 , z4 ,                   &
            xi , yi , zi , stif , dgapload ,      &
            last) bind(C, name="cpp_i7cor3")
            use iso_c_binding
#ifdef MYREAL8
            integer, parameter :: c_my_real = c_double
#else
            integer, parameter :: c_my_real = c_float
#endif
            real(c_my_real), intent(in)    :: x(3,*)                  !< global coordinate array
            integer(c_int),  intent(in)    :: irect(4,*)              !< segment connectivity
            integer(c_int),  intent(in)    :: nsv(*)                  !< slave node index array
            integer(c_int),  intent(in)    :: cand_e(*)               !< candidate element indices
            integer(c_int),  intent(in)    :: cand_n(*)               !< candidate node indices
            real(c_my_real), intent(in)    :: stf(*)                  !< element stiffness
            real(c_my_real), intent(in)    :: stfn(*)                 !< node stiffness
            real(c_my_real), intent(inout) :: gapv(*)                 !< gap values (output)
            integer(c_int),  intent(in)    :: igap                    !< gap flag
            real(c_my_real), intent(in)    :: gap                     !< global gap value
            real(c_my_real), intent(in)    :: gap_s(*)                !< slave gap values
            real(c_my_real), intent(in)    :: gap_m(*)                !< master gap values
            integer(c_int),  intent(in)    :: istf                    !< stiffness flag
            real(c_my_real), intent(in)    :: gapmin                  !< minimum gap
            real(c_my_real), intent(in)    :: gapmax                  !< maximum gap
            real(c_my_real), intent(in)    :: gap_s_l(*)              !< slave gap local values
            real(c_my_real), intent(in)    :: gap_m_l(*)              !< master gap local values
            real(c_my_real), intent(in)    :: drad                    !< search radius
            integer(c_int),  intent(inout) :: ix1(*)                  !< node 1 index
            integer(c_int),  intent(inout) :: ix2(*)                  !< node 2 index
            integer(c_int),  intent(inout) :: ix3(*)                  !< node 3 index
            integer(c_int),  intent(inout) :: ix4(*)                  !< node 4 index
            integer(c_int),  intent(inout) :: nsvg(*)                 !< gathered slave node ids
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
            real(c_my_real), intent(out)   :: xi(*)                   !< x-coord slave node
            real(c_my_real), intent(out)   :: yi(*)                   !< y-coord slave node
            real(c_my_real), intent(out)   :: zi(*)                   !< z-coord slave node
            real(c_my_real), intent(inout) :: stif(*)                 !< computed stiffness
            real(c_my_real), intent(in)    :: dgapload                !< gap load increment
            integer(c_int),  intent(in)    :: last                    !< number of candidates
          end subroutine cpp_i7cor3
        end interface

      end module cpp_i7cor3_mod
