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
!||    cppsort_mod   ../common_source/tools/sort/cppsort.F90
!||--- Description -------------------------------------------------
!||    Fortran interface to the C++ STL-sort wrappers in cppsort.cpp.
!||
!||    A single generic name  stlsort  is provided; the compiler
!||    selects the correct specific routine based on the number and
!||    types of the arguments:
!||
!||      call stlsort(len, array)          – sort real(WP) array
!||      call stlsort(len, keys, values)   – sort key-value pairs:
!||            integer keys  + integer values  → stlsort_int_int
!||            real(WP) keys + integer values  → stlsort_real_int
!||            real(WP) keys + real(WP) values → stlsort_real_real
!||
!||    iso_c_binding is used throughout so no name-mangling variants
!||    are needed in the C++ translation unit.
!||====================================================================

      module cppsort_mod

        use iso_c_binding, only : c_int
        use precision_mod,  only : WP
        implicit none
        private

        public :: stlsort

! ----------------------------------------------------------------------------------------------------------------------
!  iso_c_binding declarations – private, implementation detail
!  bind(C, name='...') pins the exact C symbol; the Fortran compiler
!  never mangles these names.
! ----------------------------------------------------------------------------------------------------------------------

        interface
          subroutine c_stlsort(len, array) bind(C, name='stlsort')
            import :: c_int, WP
            integer(c_int), intent(in)    :: len
            real(WP),       intent(inout) :: array(*)
          end subroutine c_stlsort

          subroutine c_stlsort_int_int(len, keys, values) &
              bind(C, name='stlsort_int_int')
            import :: c_int
            integer(c_int), intent(in)    :: len
            integer(c_int), intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine c_stlsort_int_int

          subroutine c_stlsort_real_int(len, keys, values) &
              bind(C, name='stlsort_real_int')
            import :: c_int, WP
            integer(c_int), intent(in)    :: len
            real(WP),       intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine c_stlsort_real_int

          subroutine c_stlsort_real_real(len, keys, values) &
              bind(C, name='stlsort_real_real')
            import :: c_int, WP
            integer(c_int), intent(in)    :: len
            real(WP),       intent(inout) :: keys(*)
            real(WP),       intent(inout) :: values(*)
          end subroutine c_stlsort_real_real
        end interface

! ----------------------------------------------------------------------------------------------------------------------
!  Generic public interface – single name stlsort, dispatch by signature
! ----------------------------------------------------------------------------------------------------------------------

        interface stlsort
          module procedure f_stlsort
          module procedure f_stlsort_int_int
          module procedure f_stlsort_real_int
          module procedure f_stlsort_real_real
        end interface stlsort

      contains

! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlsort(len, array)
          integer(c_int), intent(in)    :: len
          real(WP),       intent(inout) :: array(*)
          call c_stlsort(len, array)
        end subroutine f_stlsort

! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlsort_int_int(len, keys, values)
          integer(c_int), intent(in)    :: len
          integer(c_int), intent(inout) :: keys(*)
          integer(c_int), intent(inout) :: values(*)
          call c_stlsort_int_int(len, keys, values)
        end subroutine f_stlsort_int_int

! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlsort_real_int(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(WP),       intent(inout) :: keys(*)
          integer(c_int), intent(inout) :: values(*)
          call c_stlsort_real_int(len, keys, values)
        end subroutine f_stlsort_real_int

! ----------------------------------------------------------------------------------------------------------------------

        subroutine f_stlsort_real_real(len, keys, values)
          integer(c_int), intent(in)    :: len
          real(WP),       intent(inout) :: keys(*)
          real(WP),       intent(inout) :: values(*)
          call c_stlsort_real_real(len, keys, values)
        end subroutine f_stlsort_real_real

      end module cppsort_mod
