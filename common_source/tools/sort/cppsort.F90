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
!||    Uses iso_c_binding so the correct C symbol is resolved at link
!||    time regardless of the Fortran compiler's name-mangling scheme.
!||
!||    Available procedures:
!||      stlsort          – sort a real(WP) array in ascending order
!||      stlsort_int_int  – sort integer keys, carrying integer values
!||      stlsort_real_int – sort real(WP) keys, carrying integer values
!||      stlsort_real_real– sort real(WP) keys, carrying real(WP) values
!||      stlsort_kv       – generic: dispatches to one of the three
!||                         key-value variants based on argument types
!||====================================================================

      module cppsort_mod

        use iso_c_binding, only : c_int
        use precision_mod,  only : WP
        implicit none
        private

        public :: stlsort
        public :: stlsort_int_int
        public :: stlsort_real_int
        public :: stlsort_real_real
        public :: stlsort_kv

! ----------------------------------------------------------------------------------------------------------------------
!  iso_c_binding interface declarations
!  Each bind(C, name='...') points to the canonical (no-underscore) symbol
!  in cppsort.cpp, bypassing any compiler-specific name mangling.
! ----------------------------------------------------------------------------------------------------------------------

        interface

          !> Sort a real array of length \p len in ascending order.
          subroutine stlsort(len, array) bind(C, name='stlsort')
            import :: c_int, WP
            integer(c_int), intent(in)    :: len
            real(WP),       intent(inout) :: array(*)
          end subroutine stlsort

          !> Sort integer \p keys ascending; \p values are reordered accordingly.
          subroutine stlsort_int_int(len, keys, values) &
              bind(C, name='stlsort_int_int')
            import :: c_int
            integer(c_int), intent(in)    :: len
            integer(c_int), intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine stlsort_int_int

          !> Sort real \p keys ascending; integer \p values are reordered accordingly.
          subroutine stlsort_real_int(len, keys, values) &
              bind(C, name='stlsort_real_int')
            import :: c_int, WP
            integer(c_int), intent(in)    :: len
            real(WP),       intent(inout) :: keys(*)
            integer(c_int), intent(inout) :: values(*)
          end subroutine stlsort_real_int

          !> Sort real \p keys ascending; real \p values are reordered accordingly.
          subroutine stlsort_real_real(len, keys, values) &
              bind(C, name='stlsort_real_real')
            import :: c_int, WP
            integer(c_int), intent(in)    :: len
            real(WP),       intent(inout) :: keys(*)
            real(WP),       intent(inout) :: values(*)
          end subroutine stlsort_real_real

        end interface

! ----------------------------------------------------------------------------------------------------------------------
!  Generic interface: stlsort_kv(len, keys, values)
!  The compiler selects the specific subroutine based on the types of
!  keys and values – no explicit function suffix required at call sites.
! ----------------------------------------------------------------------------------------------------------------------

        interface stlsort_kv
          procedure :: stlsort_int_int
          procedure :: stlsort_real_int
          procedure :: stlsort_real_real
        end interface stlsort_kv

      end module cppsort_mod
