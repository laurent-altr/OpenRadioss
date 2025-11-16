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
! ======================================================================================================================
!                                                   MODULE
! ======================================================================================================================
module resol_alloc_mod
  implicit none
contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Allocate working arrays for thermal analysis in resol
!! \details This subroutine handles allocation of thermal analysis arrays including:
!!          - FTHE, FTHESKY: thermal force arrays
!!          - CONDN, CONDNSKY: thermal conductivity arrays
!!          - FTHESKYI, CONDNSKYI: thermal skyline arrays
!!          - QFRICINT: friction heat interface array
subroutine resol_alloc_thermal(glob_therm, numnod, ninter, lsky, lskyi, iparit, nthread, &
                                ifthe, icondn, fthe, fthesky, condn, condnsky,           &
                                ftheskyi, condnskyi, qfricint)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
  use glob_therm_mod, only : glob_therm_
  use precision_mod, only : wp
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  type(glob_therm_), intent(in) :: glob_therm               !< Global thermal structure
  integer, intent(in) :: numnod                             !< Number of nodes
  integer, intent(in) :: ninter                             !< Number of interfaces
  integer, intent(in) :: lsky                               !< Skyline length
  integer, intent(in) :: lskyi                              !< Implicit skyline length
  integer, intent(in) :: iparit                             !< Parallel flag
  integer, intent(in) :: nthread                            !< Number of threads
  integer, intent(inout) :: ifthe                           !< Size of FTHE array
  integer, intent(inout) :: icondn                          !< Size of CONDN array
  real(kind=wp), allocatable, intent(inout) :: fthe(:)     !< Thermal force array
  real(kind=wp), allocatable, intent(inout) :: fthesky(:)  !< Thermal force skyline
  real(kind=wp), allocatable, intent(inout) :: condn(:)    !< Thermal conductivity
  real(kind=wp), allocatable, intent(inout) :: condnsky(:) !< Conductivity skyline
  real(kind=wp), allocatable, intent(inout) :: ftheskyi(:) !< Implicit thermal force skyline
  real(kind=wp), allocatable, intent(inout) :: condnskyi(:)!< Implicit conductivity skyline
  real(kind=wp), allocatable, intent(inout) :: qfricint(:) !< Friction heat interface
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
  integer :: ierr

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
  
  ifthe  = 1
  icondn = 1
  
  if (glob_therm%itherm_fe > 0 ) then
    if (iparit == 3 ) then
      ifthe = numnod + 3*numnod*nthread
      allocate(fthe(ifthe), fthesky(lsky))
    elseif (iparit /= 0 ) then
      ifthe = numnod
      allocate(fthe(ifthe), fthesky(lsky))
    else
      ifthe = numnod*nthread
      allocate(fthe(ifthe), fthesky(0))
    endif
    
    allocate(qfricint(ninter))
    qfricint(1:ninter) = 0.0_wp
    
    if (glob_therm%nodadt_therm == 1) then
      if (iparit == 0 ) then
        icondn = numnod*nthread
        allocate (condn(icondn), condnsky(0))
      else
        icondn = numnod
        allocate (condn(icondn), stat=ierr)
        allocate (condnsky(lsky), stat=ierr)
      endif
    endif
  else
    ifthe  = 1
    icondn = 1
    allocate(fthe(ifthe), fthesky(0))
    allocate(qfricint(ninter))
    qfricint(1:ninter) = 0.0_wp
    allocate(condn(icondn), condnsky(0))
  endif
  
  ! Implicit thermal arrays
  if (glob_therm%intheat > 0) then
    if (iparit /= 0) then
      allocate(ftheskyi(lskyi))
      ftheskyi(1:lskyi) = 0.0_wp
    else
      allocate(ftheskyi(0))
    endif
    
    if (glob_therm%nodadt_therm == 1) then
      if (iparit /= 0) then
        allocate(condnskyi(lskyi))
      else
        allocate(condnskyi(0))
      endif
    else
      allocate(condnskyi(0))
    endif
  else
    allocate(ftheskyi(0))
    allocate(condnskyi(0))
  endif

! ----------------------------------------------------------------------------------------------------------------------
end subroutine resol_alloc_thermal

end module resol_alloc_mod
