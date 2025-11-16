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

!! \brief Allocate working arrays for ALE (Arbitrary Lagrangian Eulerian) formulation
!! \details This subroutine handles allocation of ALE-related arrays including:
!!          - AFLOW: ALE flow acceleration
!!          - FFSKY, IFOAM, IFOAM_CONT: ALE force and foam arrays
subroutine resol_alloc_ale(ialelag, iparit, numnod, nthread, lsky, &
                            aflow, ffsky, ifoam, ifoam_cont, vflow, dflow, wflow)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
  use precision_mod, only : wp
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  integer, intent(in) :: ialelag                            !< ALE flag
  integer, intent(in) :: iparit                             !< Parallel flag
  integer, intent(in) :: numnod                             !< Number of nodes
  integer, intent(in) :: nthread                            !< Number of threads
  integer, intent(in) :: lsky                               !< Skyline length
  real(kind=wp), allocatable, intent(inout) :: aflow(:)    !< ALE flow acceleration
  real(kind=wp), allocatable, intent(inout) :: ffsky(:)    !< ALE force skyline
  integer, allocatable, intent(inout) :: ifoam(:)          !< Foam indicator
  integer, allocatable, intent(inout) :: ifoam_cont(:)     !< Foam contact indicator
  real(kind=wp), allocatable, intent(inout) :: vflow(:)    !< Flow velocity
  real(kind=wp), allocatable, intent(inout) :: dflow(:)    !< Flow displacement
  real(kind=wp), allocatable, intent(inout) :: wflow(:)    !< Flow work

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
  
  if (ialelag > 0) then
    if (iparit == 0) then
      allocate (aflow(3*numnod*nthread))
      allocate(ffsky(0))
      allocate(ifoam(numnod*nthread))
      allocate(ifoam_cont(numnod*nthread))
    else
      allocate (aflow(3*numnod))
      allocate(ffsky(3*lsky))
      allocate(ifoam(numnod))
      allocate(ifoam_cont(numnod))
      ffsky = 0.0_wp
    endif
    aflow = 0.0_wp
    ifoam = 0
    ifoam_cont = 0
  else
    allocate(aflow(0), vflow(0), dflow(0), wflow(0), ffsky(0), ifoam(0), ifoam_cont(0))
  endif

! ----------------------------------------------------------------------------------------------------------------------
end subroutine resol_alloc_ale

!! \brief Allocate working arrays for adaptive meshing (ADMESH)
!! \details This subroutine handles allocation of ADMESH-related arrays
subroutine resol_alloc_admesh(nadmesh, numelc, numeltg, levelmax, iparit, numnod, &
                                lsh4act, lsh4kin, psh4act, psh4kin,                 &
                                lsh3act, lsh3kin, psh3act, psh3kin,                 &
                                msh4sky, msh3sky, ilevnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
  use message_mod
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  integer, intent(in) :: nadmesh                            !< Adaptive mesh flag
  integer, intent(in) :: numelc                             !< Number of shell elements
  integer, intent(in) :: numeltg                            !< Number of triangular elements
  integer, intent(in) :: levelmax                           !< Maximum refinement level
  integer, intent(in) :: iparit                             !< Parallel flag
  integer, intent(in) :: numnod                             !< Number of nodes
  integer, allocatable, intent(inout) :: lsh4act(:)        !< Shell4 active list
  integer, allocatable, intent(inout) :: lsh4kin(:)        !< Shell4 kinematic list
  integer, allocatable, intent(inout) :: psh4act(:)        !< Shell4 active pointer
  integer, allocatable, intent(inout) :: psh4kin(:)        !< Shell4 kinematic pointer
  integer, allocatable, intent(inout) :: lsh3act(:)        !< Shell3 active list
  integer, allocatable, intent(inout) :: lsh3kin(:)        !< Shell3 kinematic list
  integer, allocatable, intent(inout) :: psh3act(:)        !< Shell3 active pointer
  integer, allocatable, intent(inout) :: psh3kin(:)        !< Shell3 kinematic pointer
  real, allocatable, intent(inout) :: msh4sky(:)           !< Shell4 mass skyline
  real, allocatable, intent(inout) :: msh3sky(:)           !< Shell3 mass skyline
  integer, allocatable, intent(inout) :: ilevnod(:)        !< Node refinement level

! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
  integer :: ierror, ierr

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
  
  if (nadmesh /= 0) then
    allocate(lsh4act(numelc), lsh4kin(numelc),                              &
             psh4act(0:levelmax+1), psh4kin(0:levelmax+1),                  &
             lsh3act(numeltg), lsh3kin(numeltg),                            &
             psh3act(0:levelmax+1), psh3kin(0:levelmax+1),                  &
             stat=ierror)
    
    if (ierror /= 0) then
      call ancmsg(msgid=153, anmode=aninfo, i1=ierror)
      call arret(2)
    end if
    
    if (iparit /= 0) then
      allocate(msh4sky(numelc), msh3sky(numeltg), stat=ierror)
      if (ierror /= 0) then
        call ancmsg(msgid=153, anmode=aninfo, i1=ierror)
        call arret(2)
      end if
    else
      allocate(msh4sky(0), msh3sky(0))
    end if
    
    allocate(ilevnod(0:numnod), stat=ierr)
    if (ierr /= 0) then
      call ancmsg(msgid=20, anmode=aninfo)
      call arret(2)
    end if
  end if

! ----------------------------------------------------------------------------------------------------------------------
end subroutine resol_alloc_admesh

end module resol_alloc_mod
