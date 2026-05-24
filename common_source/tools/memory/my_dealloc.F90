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

! ======================================================================================================================
! fypp template — generates my_dealloc.F90
! Do NOT edit the generated my_dealloc.F90 directly; edit this file and re-run fypp.
!
! Deallocation counterpart to my_alloc.fy.
!
! Each generated subroutine:
!   1. Guards against double-free (checks allocated / associated).
!   2. Resolves the pointer address with c_loc of the first element.
!   3. Calls cpp_record_dealloc_addr(addr) — the C++ side looks up the address in
!      the per-allocation map, subtracts the previously recorded byte count from the
!      per-site counter, and removes the address entry.  One site name can have many
!      live addresses simultaneously (same allocation site called from many callers).
!   4. Deallocates the array.  For pointer arrays, also nullifies after free.
!
! Axes of variation:
!   TYPES     : (fortran_type, short_name)  — same set as my_alloc.fy
!   MEM_KINDS : (fortran_attr, name_prefix) — 'allocatable' / 'pointer'
!   RANKS     : (rank, dim_var_list)        — 1D / 2D / 3D
!
! No IDX_KINDS axis: deallocation takes only the array, no dimension arguments.
!
! Subroutine naming convention:
!   my_dealloc_<mem_prefix><type_name>_<rank>d
!   e.g. my_dealloc_pdouble_3d  =>  pointer, double precision, 3D
!
! Placeholder for derived types is at the bottom of the contains section.
! ======================================================================================================================



      module my_dealloc_mod
        use iso_c_binding, only : c_ptr, c_loc
        implicit none

        interface
          subroutine cpp_record_dealloc_addr(addr) bind(C, name="cpp_record_dealloc_addr")
            import :: c_ptr
            type(c_ptr), value, intent(in) :: addr
          end subroutine cpp_record_dealloc_addr
        end interface

        private :: my_dealloc_real_1d
        private :: my_dealloc_real_2d
        private :: my_dealloc_real_3d
        private :: my_dealloc_double_1d
        private :: my_dealloc_double_2d
        private :: my_dealloc_double_3d
        private :: my_dealloc_integer_1d
        private :: my_dealloc_integer_2d
        private :: my_dealloc_integer_3d
        private :: my_dealloc_logical_1d
        private :: my_dealloc_logical_2d
        private :: my_dealloc_logical_3d
        private :: my_dealloc_preal_1d
        private :: my_dealloc_preal_2d
        private :: my_dealloc_preal_3d
        private :: my_dealloc_pdouble_1d
        private :: my_dealloc_pdouble_2d
        private :: my_dealloc_pdouble_3d
        private :: my_dealloc_pinteger_1d
        private :: my_dealloc_pinteger_2d
        private :: my_dealloc_pinteger_3d
        private :: my_dealloc_plogical_1d
        private :: my_dealloc_plogical_2d
        private :: my_dealloc_plogical_3d

        public :: my_dealloc

        interface my_dealloc
          module procedure my_dealloc_real_1d
          module procedure my_dealloc_real_2d
          module procedure my_dealloc_real_3d
          module procedure my_dealloc_double_1d
          module procedure my_dealloc_double_2d
          module procedure my_dealloc_double_3d
          module procedure my_dealloc_integer_1d
          module procedure my_dealloc_integer_2d
          module procedure my_dealloc_integer_3d
          module procedure my_dealloc_logical_1d
          module procedure my_dealloc_logical_2d
          module procedure my_dealloc_logical_3d
          module procedure my_dealloc_preal_1d
          module procedure my_dealloc_preal_2d
          module procedure my_dealloc_preal_3d
          module procedure my_dealloc_pdouble_1d
          module procedure my_dealloc_pdouble_2d
          module procedure my_dealloc_pdouble_3d
          module procedure my_dealloc_pinteger_1d
          module procedure my_dealloc_pinteger_2d
          module procedure my_dealloc_pinteger_3d
          module procedure my_dealloc_plogical_1d
          module procedure my_dealloc_plogical_2d
          module procedure my_dealloc_plogical_3d
        end interface my_dealloc

      contains

        subroutine record_dealloc_addr(addr)
          type(c_ptr), intent(in) :: addr
          call cpp_record_dealloc_addr(addr)
        end subroutine record_dealloc_addr

! ======================================================================================================================
!                                     GENERATED DEALLOCATION ROUTINES
!   Loop order: MEM_KINDS x TYPES x RANKS
!   MEM_KINDS : allocatable (''), pointer ('p')
!   TYPES     : real, double precision, integer, logical
!   RANKS     : 1d, 2d, 3d
! ======================================================================================================================

!! \brief Deallocate a 1D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_1d(a)
          real, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_1d

!! \brief Deallocate a 2D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_2d(a)
          real, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_2d

!! \brief Deallocate a 3D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_3d(a)
          real, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_3d

!! \brief Deallocate a 1D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_1d(a)
          double precision, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_1d

!! \brief Deallocate a 2D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_2d(a)
          double precision, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_2d

!! \brief Deallocate a 3D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_3d(a)
          double precision, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_3d

!! \brief Deallocate a 1D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_1d(a)
          integer, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_1d

!! \brief Deallocate a 2D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_2d(a)
          integer, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_2d

!! \brief Deallocate a 3D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_3d(a)
          integer, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_3d

!! \brief Deallocate a 1D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_1d(a)
          logical, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_1d

!! \brief Deallocate a 2D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_2d(a)
          logical, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_2d

!! \brief Deallocate a 3D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_3d(a)
          logical, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_3d

!! \brief Deallocate a 1D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_1d(a)
          real, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_1d

!! \brief Deallocate a 2D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_2d(a)
          real, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_2d

!! \brief Deallocate a 3D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_3d(a)
          real, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_3d

!! \brief Deallocate a 1D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_1d(a)
          double precision, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_1d

!! \brief Deallocate a 2D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_2d(a)
          double precision, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_2d

!! \brief Deallocate a 3D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_3d(a)
          double precision, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_3d

!! \brief Deallocate a 1D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_1d(a)
          integer, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_1d

!! \brief Deallocate a 2D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_2d(a)
          integer, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_2d

!! \brief Deallocate a 3D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_3d(a)
          integer, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_3d

!! \brief Deallocate a 1D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_1d(a)
          logical, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_1d

!! \brief Deallocate a 2D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_2d(a)
          logical, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_2d

!! \brief Deallocate a 3D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_3d(a)
          logical, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_3d


! ======================================================================================================================
!                            PLACEHOLDER — DERIVED TYPE DEALLOCATION ROUTINES
!
! To add derived type support, add the type to the TYPES set above, or add a dedicated
! interface block below for types that require special teardown before dealloc.
! ======================================================================================================================

      end module my_dealloc_mod
