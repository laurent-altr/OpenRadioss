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
module resol_alloc_connectivity_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Allocate connectivity and element state arrays
!! \details This routine allocates inverse connectivity arrays (CNEL, ADDCNEL, ADDTMPL) and element state tracking arrays
  subroutine resol_alloc_connectivity(idel7ng, irad2r, ialemuscl, pdel, numnod, lcnel, &
                                       numels, numelq, numelc, numelt, numelp, numelr, numeltg, &
                                       npart, size_addcnel, size_cnel, neleml, s_elem_state, &
                                       ierror, cnel, addcnel, addtmpl, tagel, ipartl, partsav2, &
                                       elem_state, alemuscl_pcnel, alemuscl_paddcnel, alemuscl_paddtmpl)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    integer, intent(in) :: idel7ng              !< Element deletion flag
    integer, intent(in) :: irad2r               !< RAD2RAD flag
    integer, intent(in) :: ialemuscl            !< ALEMUSCL flag
    integer, intent(in) :: pdel                 !< Pressure deletion flag
    integer, intent(in) :: numnod               !< Number of nodes
    integer, intent(in) :: lcnel                !< Size of connectivity array
    integer, intent(in) :: numels               !< Number of solid elements
    integer, intent(in) :: numelq               !< Number of quad elements
    integer, intent(in) :: numelc               !< Number of shell elements
    integer, intent(in) :: numelt               !< Number of truss elements
    integer, intent(in) :: numelp               !< Number of beam elements
    integer, intent(in) :: numelr               !< Number of spring elements
    integer, intent(in) :: numeltg              !< Number of triangle elements
    integer, intent(in) :: npart                !< Number of parts
    integer, intent(out) :: size_addcnel        !< Size of ADDCNEL array
    integer, intent(out) :: size_cnel           !< Size of CNEL array
    integer, intent(out) :: neleml              !< Number of local elements
    integer, intent(out) :: s_elem_state        !< Size of element state array
    integer, intent(out) :: ierror              !< Error flag
    integer, allocatable, target, intent(out) :: cnel(:)         !< Connectivity array
    integer, allocatable, target, intent(out) :: addcnel(:)      !< Address connectivity array
    integer, allocatable, target, intent(out) :: addtmpl(:)      !< Temporary address array
    integer, allocatable, intent(out) :: tagel(:)        !< Element tag array
    integer, allocatable, intent(out) :: ipartl(:)       !< Local part array
    real(kind=8), allocatable, intent(out) :: partsav2(:,:)      !< Part save array (use precision from parent)
    logical, allocatable, intent(out) :: elem_state(:)   !< Element state array
    integer, pointer, intent(out) :: alemuscl_pcnel(:)     !< Pointer to CNEL
    integer, pointer, intent(out) :: alemuscl_paddcnel(:)  !< Pointer to ADDCNEL
    integer, pointer, intent(out) :: alemuscl_paddtmpl(:)  !< Pointer to ADDTMPL
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
    integer :: ierror2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------

    if ((idel7ng > 0) .or. (irad2r /= 0) .or. (ialemuscl > 0) .or. (pdel > 0)) then
      size_addcnel = numnod + 1
      size_cnel = lcnel
      neleml = numels + numelq + numelc + numelt + numelp + numelr + numeltg
      s_elem_state = neleml
    else
      size_addcnel = 0
      size_cnel = 0
      s_elem_state = 0
    end if

    ierror = 0
    allocate(cnel(0:size_cnel))
    allocate(addcnel(0:size_addcnel))

    if ((idel7ng > 0) .or. (irad2r /= 0) .or. (ialemuscl > 0) .or. (pdel > 0)) then
      ! Allocation of inverse connectivity array
      allocate(addtmpl(0:numnod+1))
      ierror = ierror + ierror2
      neleml = numels + numelq + numelc + numelt + numelp + numelr + numeltg
      allocate(tagel(1:neleml))
      tagel(:) = 0
      alemuscl_pcnel => cnel
      alemuscl_paddcnel => addcnel
      alemuscl_paddtmpl => addtmpl
    else
      allocate(addtmpl(0), tagel(0))
    end if

    allocate(ipartl(npart))
    allocate(partsav2(2,npart))
    allocate(elem_state(s_elem_state))
    elem_state(1:s_elem_state) = .true.

! ----------------------------------------------------------------------------------------------------------------------
  end subroutine resol_alloc_connectivity
end module resol_alloc_connectivity_mod
