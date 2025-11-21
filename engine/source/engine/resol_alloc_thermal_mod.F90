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
module resol_alloc_thermal_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Allocate thermal analysis arrays
!! \details This routine allocates working arrays for thermal analysis including heat transfer and nodal time step
  subroutine resol_alloc_thermal(numnod, ninter, nodadt, itherm_fe, idt_therm, &
                                  intheat, iparit, nthread, lsky, lskyi, &
                                  nodadt_therm, ifthe, icondn, icodt0, icodr0, &
                                  mcp_off, fthe, fthesky, qfricint, condn, condnsky, &
                                  ftheskyi, condnskyi)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
    use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    integer, intent(in) :: numnod                !< Number of nodes
    integer, intent(in) :: ninter                !< Number of interfaces
    integer, intent(in) :: nodadt                !< Nodal time step flag
    integer, intent(in) :: itherm_fe             !< Thermal finite element flag
    integer, intent(in) :: idt_therm             !< Thermal time step flag
    integer, intent(in) :: intheat               !< Internal heat flag
    integer, intent(in) :: iparit                !< Parallel assembly flag
    integer, intent(in) :: nthread               !< Number of threads
    integer, intent(in) :: lsky                  !< Size of skyline array
    integer, intent(in) :: lskyi                 !< Size of interface skyline array
    integer, intent(out) :: nodadt_therm         !< Nodal thermal time step flag
    integer, intent(out) :: ifthe                !< Size of thermal force array
    integer, intent(out) :: icondn               !< Size of conductivity array
    real(kind=WP), allocatable, intent(out) :: icodt0(:)    !< Initial translational kinematic for thermal time step
    real(kind=WP), allocatable, intent(out) :: icodr0(:)    !< Initial rotational kinematic for thermal time step
    real(kind=WP), allocatable, intent(out) :: mcp_off(:)   !< Mass specific heat capacity offset
    real(kind=WP), allocatable, intent(out) :: fthe(:)      !< Thermal force array
    real(kind=WP), allocatable, intent(out) :: fthesky(:)   !< Thermal force skyline array
    real(kind=WP), allocatable, intent(out) :: qfricint(:)  !< Interface friction heat array
    real(kind=WP), allocatable, intent(out) :: condn(:)     !< Conductivity array
    real(kind=WP), allocatable, intent(out) :: condnsky(:)  !< Conductivity skyline array
    real(kind=WP), allocatable, intent(out) :: ftheskyi(:)  !< Interface thermal force skyline array
    real(kind=WP), allocatable, intent(out) :: condnskyi(:) !< Interface conductivity skyline array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
    integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------

    ! Working arrays for thermal analysis
    nodadt_therm = 0
    if (idt_therm == 1) then
      if (ninter > 0 .or. nodadt > 0) nodadt_therm = 1  ! Flag for nodal thermal time step
    end if

    if (idt_therm == 1) then
      allocate(icodt0(numnod))  ! Tabs for storing initial kinematic when thermal time step
      allocate(icodr0(numnod))
    else
      allocate(icodt0(0))
      allocate(icodr0(0))
    end if

    allocate(mcp_off(numnod))
    mcp_off(1:numnod) = 1.0_WP

    ifthe = 1
    icondn = 1
    if (itherm_fe > 0) then
      if (iparit == 3) then
        ifthe = numnod + 3*numnod*nthread
        allocate(fthe(ifthe), fthesky(lsky))
      elseif (iparit /= 0) then
        ifthe = numnod
        allocate(fthe(ifthe), fthesky(lsky))
      else
        ifthe = numnod*nthread
        allocate(fthe(ifthe), fthesky(0))
      end if
      allocate(qfricint(ninter))
      qfricint(1:ninter) = 0.0_WP
      if (nodadt_therm == 1) then
        if (iparit == 0) then
          icondn = numnod*nthread
          allocate(condn(icondn), condnsky(0))
        else
          icondn = numnod
          allocate(condn(icondn), stat=ierr)
          allocate(condnsky(lsky), stat=ierr)
        end if
      end if
    else
      ifthe = 1
      icondn = 1
      allocate(fthe(ifthe), fthesky(0))
      allocate(qfricint(ninter))
      qfricint(1:ninter) = 0.0_WP
      allocate(condn(icondn), condnsky(0))
    end if

    if (intheat > 0) then
      if (iparit /= 0) then
        allocate(ftheskyi(lskyi))
        ftheskyi(1:lskyi) = 0.0_WP
      else
        allocate(ftheskyi(0))
      end if
      if (nodadt_therm == 1) then
        if (iparit /= 0) then
          allocate(condnskyi(lskyi))
        else
          allocate(condnskyi(0))
        end if
      else
        allocate(condnskyi(0))
      end if
    else
      allocate(ftheskyi(0))
      allocate(condnskyi(0))
    end if

! ----------------------------------------------------------------------------------------------------------------------
  end subroutine resol_alloc_thermal
end module resol_alloc_thermal_mod
