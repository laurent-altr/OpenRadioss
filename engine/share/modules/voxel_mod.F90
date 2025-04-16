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
    subroutine c_voxel_initialize(v, irect, nrtm, gap, nsv, nsn, X, numnod) bind(C, name="Voxel_initialize")
      import :: c_ptr, c_int, c_double, c_float
      implicit none
      type(c_ptr), value :: v
      integer(c_int), intent(in) :: irect(*)
      integer(c_int), value :: nrtm
      real(my_real_kind), intent(in) :: gap(*)
      integer(c_int), intent(in) :: nsv(*)
      integer(c_int), value :: nsn
      real(my_real_kind), intent(in) :: X(*)
      integer(c_int), value :: numnod
    end subroutine c_voxel_initialize

    ! Voxel_delete
    subroutine c_voxel_delete(v) bind(C, name="Voxel_delete")
      import :: c_ptr
      implicit none
      type(c_ptr), value :: v
    end subroutine c_voxel_delete

    ! Voxel_update_node
    subroutine c_voxel_update_node(v, x, y, z, nodeId) bind(C, name="Voxel_update_node")
      import :: c_ptr, c_double, c_int
      implicit none
      type(c_ptr), value :: v
      real(c_double), value :: x, y, z
      integer(c_int), value :: nodeId
    end subroutine c_voxel_update_node

    ! Voxel_update_surf
    subroutine c_voxel_update_surf(v, xmin, ymin, zmin, xmax, ymax, zmax, surfId) bind(C, name="Voxel_update_surf")
      import :: c_ptr, c_double, c_int
      implicit none
      type(c_ptr), value :: v
      real(c_double), value :: xmin, ymin, zmin, xmax, ymax, zmax
      integer(c_int), value :: surfId
    end subroutine c_voxel_update_surf

    ! Voxel_get_max_candidates
    function c_voxel_get_max_candidates(v) bind(C, name="Voxel_get_max_candidates")
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), value :: v
      integer(c_int) :: c_voxel_get_max_candidates
    end function c_voxel_get_max_candidates

    ! Voxel_get_candidates
    subroutine c_voxel_get_candidates(v, ne, cands, nb) bind(C, name="Voxel_get_candidates")
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), value :: v
      integer(c_int), value :: ne
      integer(c_int), intent(out) :: cands(*)
      integer(c_int) :: nb
    end subroutine c_voxel_get_candidates

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

  ! Initialize a Voxel
  subroutine voxel_initialize(voxel, irect, nrtm, gap, nsv, nsn, X, numnod)
    type(c_ptr), intent(inout) :: voxel
    integer, intent(in) :: nrtm
    integer, intent(in) :: irect(4,nrtm)  ! Assuming it's dimensioned as (4,nrtm)
    real(my_real_kind), intent(in) :: gap(nrtm)
    integer, intent(in) :: nsn
    integer, intent(in) :: nsv(nsn)
    integer, intent(in) :: numnod
    real(my_real_kind), intent(in) :: X(3,numnod)  ! Assuming it's dimensioned as (3*numnod)
    
    call c_voxel_initialize(voxel, irect, nrtm, gap, nsv, nsn, X, numnod)
  end subroutine voxel_initialize

  ! Delete a Voxel object
  subroutine voxel_delete(voxel)
    type(c_ptr), intent(inout) :: voxel
    
    call c_voxel_delete(voxel)
    voxel = c_null_ptr
  end subroutine voxel_delete

  ! Update a node in a Voxel
  subroutine voxel_update_node(voxel, x, y, z, nodeId)
    type(c_ptr), intent(inout) :: voxel
    real(c_double), intent(in) :: x, y, z
    integer, intent(in) :: nodeId
    
    call c_voxel_update_node(voxel, x, y, z, nodeId)
  end subroutine voxel_update_node

  ! Update a surface in a Voxel
  subroutine voxel_update_surf(voxel, xmin, ymin, zmin, xmax, ymax, zmax, surfId)
    type(c_ptr), intent(inout) :: voxel
    real(c_double), intent(in) :: xmin, ymin, zmin, xmax, ymax, zmax
    integer, intent(in) :: surfId
    
    call c_voxel_update_surf(voxel, xmin, ymin, zmin, xmax, ymax, zmax, surfId)
  end subroutine voxel_update_surf

  ! Get the maximum number of candidates for a Voxel
  function voxel_get_max_candidates(voxel) result(max_candidates)
    type(c_ptr), intent(inout) :: voxel
    integer :: max_candidates
    
    max_candidates = c_voxel_get_max_candidates(voxel)
  end function voxel_get_max_candidates

  ! Get the candidates for a surface in a Voxel
  subroutine voxel_get_candidates(voxel, ne, cands, nb)
    type(c_ptr), intent(inout) :: voxel
    integer, intent(in) :: ne
    integer, intent(inout) :: cands(*)
    integer, intent(out) :: nb
    
    call c_voxel_get_candidates(voxel, ne, cands, nb)
  end subroutine voxel_get_candidates

end module voxel_mod