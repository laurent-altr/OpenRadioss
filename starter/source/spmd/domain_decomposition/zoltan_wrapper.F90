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
module zoltan_interface
  use iso_c_binding
  implicit none
  
  ! Zoltan partitioning methods
  integer, parameter :: ZOLTAN_METHOD_GRAPH = 0
  integer, parameter :: ZOLTAN_METHOD_HYPERGRAPH = 1  ! Best for 3-6 constraints
  integer, parameter :: ZOLTAN_METHOD_RCB = 2         ! Geometric, naturally contiguous
  integer, parameter :: ZOLTAN_METHOD_HSFC = 3        ! Space-filling curve, naturally contiguous
  
  ! Contiguity enforcement levels
  integer, parameter :: CONTIGUITY_DISABLED = 0
  integer, parameter :: CONTIGUITY_REPORT = 1
  integer, parameter :: CONTIGUITY_ENFORCE = 2
  
  interface
    
    ! Full Zoltan partition with contiguity control
    function wrap_zoltan_partition_contiguous(nelem, ncond, xadj, adjncy, &
                                             iwd, adjwgt, nnode, imbalance, method, &
                                             enforce_contiguity, nec, cep) &
      bind(C, name="wrap_zoltan_partition_contiguous")
      use iso_c_binding
      integer(c_int) :: wrap_zoltan_partition_contiguous
      integer(c_int), intent(in) :: nelem, ncond, nnode, method, enforce_contiguity
      integer(c_int), intent(in) :: xadj(*), adjncy(*), iwd(*)
      integer(c_int), intent(in) :: adjwgt(*)
      real(c_float), intent(in) :: imbalance
      integer(c_int), intent(out) :: nec
      integer(c_int), intent(out) :: cep(*)
    end function wrap_zoltan_partition_contiguous
    
    ! Simplified multi-constraint partition (HYPERGRAPH method with contiguity reporting)
    function wrap_zoltan_partition_multiconstraint(nelem, ncond, xadj, adjncy, &
                                                  iwd, nnode, imbalance, &
                                                  nec, cep) &
      bind(C, name="wrap_zoltan_partition_multiconstraint")
      use iso_c_binding
      integer(c_int) :: wrap_zoltan_partition_multiconstraint
      integer(c_int), intent(in) :: nelem, ncond, nnode
      integer(c_int), intent(in) :: xadj(*), adjncy(*), iwd(*)
      real(c_float), intent(in) :: imbalance
      integer(c_int), intent(out) :: nec
      integer(c_int), intent(out) :: cep(*)
    end function wrap_zoltan_partition_multiconstraint
    
  end interface
  
contains
  
  ! Helper function optimized for large meshes with 3-6 constraints
  ! This is the recommended interface for your use case
  function zoltan_partition_multiconstraint(nelem, ncond, xadj, adjncy, iwd, &
                                           nnode, imbalance, nec, cep) result(ierr)
    integer, intent(in) :: nelem, ncond, nnode
    integer, intent(in) :: xadj(nelem+1), adjncy(*)
    integer, intent(in) :: iwd(nelem*ncond)
    real, intent(in) :: imbalance
    integer, intent(out) :: nec
    integer, intent(out) :: cep(nelem)
    integer :: ierr
    
    real(c_float) :: c_imbalance
    integer(c_int) :: c_nelem, c_ncond, c_nnode, c_nec
    
    c_nelem = nelem
    c_ncond = ncond
    c_nnode = nnode
    c_imbalance = imbalance
    
    ! Use HYPERGRAPH method with contiguity reporting
    ierr = wrap_zoltan_partition_multiconstraint(c_nelem, c_ncond, xadj, adjncy, &
                                                iwd, c_nnode, c_imbalance, &
                                                c_nec, cep)
    nec = c_nec
    
  end function zoltan_partition_multiconstraint
  
! ! Advanced interface with full control over method and contiguity
! function zoltan_partition_advanced(nelem, ncond, xadj, adjncy, iwd, &
!                                   nnode, imbalance, method, check_contiguity, &
!                                   nec, cep) result(ierr)
!   integer, intent(in) :: nelem, ncond, nnode
!   integer, intent(in) :: xadj(nelem+1), adjncy(*)
!   integer, intent(in) :: iwd(nelem*ncond)
!   real, intent(in) :: imbalance
!   integer, intent(in) :: method
!   logical, intent(in) :: check_contiguity
!   integer, intent(out) :: nec
!   integer, intent(out) :: cep(nelem)
!   integer :: ierr
!   
!   real(c_float) :: c_imbalance
!   integer(c_int) :: c_nelem, c_ncond, c_nnode, c_nec
!   integer(c_int) :: c_method, c_contiguity
!   
!   c_nelem = nelem
!   c_ncond = ncond
!   c_nnode = nnode
!   c_imbalance = imbalance
!   c_method = method
!   c_contiguity = merge(CONTIGUITY_REPORT, CONTIGUITY_DISABLED, check_contiguity)
!   
!   ierr = wrap_zoltan_partition_contiguous(c_nelem, c_ncond, xadj, adjncy, &
!                                          iwd, C_NULL_PTR, c_nnode, c_imbalance, &
!                                          c_method, c_contiguity, c_nec, cep)
!   nec = c_nec
!   
! end function zoltan_partition_advanced
  
end module zoltan_interface