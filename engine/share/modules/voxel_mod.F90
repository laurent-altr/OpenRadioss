!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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


! Define the Fortran interface to the C++ Voxel class functions

! This module provides Fortran bindings to the Voxel C++ class.
module voxel_mod
  use, intrinsic :: iso_c_binding
  implicit none


#include "my_real.inc"  
#ifdef MYREAL8
#define my_real_kind c_double
#else
#define my_real_kind c_float
#endif


  ! Interface declarations for C functions
  interface

    ! Voxel_new
    function c_voxel_new(nbx, nby, nbz, nbsurfaces, nbnodes) bind(C, name="Voxel_new")
      import :: c_ptr, c_int
      implicit none
      integer(c_int), value :: nbx, nby, nbz, nbsurfaces, nbnodes
      type(c_ptr) :: c_voxel_new
    end function c_voxel_new

    ! Voxel_set_bounds
    subroutine c_voxel_set_bounds(v, xmin, ymin, zmin, xmax, ymax, zmax) bind(C, name="Voxel_set_bounds")
      import :: c_ptr, c_double
      implicit none
      type(c_ptr), value :: v
      real(c_double), value :: xmin, ymin, zmin, xmax, ymax, zmax
    end subroutine c_voxel_set_bounds

    ! Voxel_initialize
    subroutine c_voxel_initialize(v, irect, nrtm, gap, nsv, nsn, X, numnod, stf, stfn) bind(C, name="Voxel_initialize")
      import :: c_ptr, c_int, c_double, c_float
      implicit none
      type(c_ptr), value :: v
      integer(c_int), intent(in) :: irect(*)
      integer(c_int), value :: nrtm
      real(my_real_kind), intent(in) :: gap(*)
      integer(c_int), intent(in) :: nsv(*)
      integer(c_int), value :: nsn
      real(my_real_kind), intent(in) :: X(*)
      real(my_real_kind), intent(in):: stf(*), stfn(*)
      integer(c_int), value :: numnod
    end subroutine c_voxel_initialize
    subroutine c_voxel_update(v, irect, nrtm, gap, nsv, nsn, X, numnod, stf, stfn) bind(C, name="Voxel_update")
      import :: c_ptr, c_int, c_double, c_float
      implicit none
      type(c_ptr), value :: v
      integer(c_int), intent(in) :: irect(*)
      integer(c_int), value :: nrtm
      real(my_real_kind), intent(in) :: gap(*)
      integer(c_int), intent(in) :: nsv(*)
      integer(c_int), value :: nsn
      real(my_real_kind), intent(in) :: X(*)
      real(my_real_kind), intent(in):: stf(*), stfn(*)
      integer(c_int), value :: numnod
    end subroutine c_voxel_update


    ! Voxel_delete
    subroutine c_voxel_delete(v) bind(C, name="Voxel_delete")
      import :: c_ptr
      implicit none
      type(c_ptr), value :: v
    end subroutine c_voxel_delete



    ! Voxel_get_max_candidates
    function c_voxel_get_max_candidates(v) bind(C, name="Voxel_get_max_candidates")
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), value :: v
      integer(c_int) :: c_voxel_get_max_candidates
    end function c_voxel_get_max_candidates

    ! Voxel_get_candidates
    subroutine c_voxel_get_candidates(v, ne, cands, nb) bind(C, name="Voxel_get_candidates_data")
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), value :: v
      integer(c_int), value :: ne
      type(c_ptr), intent(out) :: cands
      integer(c_int) :: nb
    end subroutine c_voxel_get_candidates

    subroutine c_voxel_get_bounds(v, xmin, ymin, zmin, xmax, ymax, zmax) bind(C, name="Voxel_get_bounds")
      import :: c_ptr, c_double
      implicit none
      type(c_ptr), value :: v
      real(c_double), intent(out) :: xmin, ymin, zmin, xmax, ymax, zmax
    end subroutine c_voxel_get_bounds

    subroutine c_voxel_restart(v, nbx, nby, nbz, nbsurfaces, nbnodes) bind(C, name="Voxel_restart")
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), value :: v
      integer(c_int), value :: nbx, nby, nbz, nbsurfaces, nbnodes
    end subroutine c_voxel_restart





  end interface

contains

  ! Fortran wrapper functions (with more Fortran-friendly interfaces)

  ! Create a new Voxel object
  function voxel_new(nbx, nby, nbz, nbsurfaces, nbnodes) result(voxel)
    integer, intent(inout) :: nbx, nby, nbz, nbsurfaces, nbnodes
    type(c_ptr) :: voxel
    
    voxel = c_voxel_new(nbx, nby, nbz, nbsurfaces, nbnodes)
  end function voxel_new

  ! Set the bounds of a Voxel
  subroutine voxel_set_bounds(voxel, xmin, ymin, zmin, xmax, ymax, zmax)
    type(c_ptr), intent(inout) :: voxel
    real(c_double), intent(in) :: xmin, ymin, zmin, xmax, ymax, zmax
    
    call c_voxel_set_bounds(voxel, xmin, ymin, zmin, xmax, ymax, zmax)
  end subroutine voxel_set_bounds

  ! Delete a Voxel object
  subroutine voxel_delete(voxel)
    type(c_ptr), intent(inout) :: voxel
    
    call c_voxel_delete(voxel)
    voxel = c_null_ptr
  end subroutine voxel_delete


  ! Get the maximum number of candidates for a Voxel
  function voxel_get_max_candidates(voxel) result(max_candidates)
    type(c_ptr), intent(inout) :: voxel
    integer :: max_candidates
    
    max_candidates = c_voxel_get_max_candidates(voxel)
  end function voxel_get_max_candidates

end module voxel_mod