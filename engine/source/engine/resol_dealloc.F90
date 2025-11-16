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
module resol_dealloc_mod
  implicit none
contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Deallocate working arrays for thermal analysis in resol
!! \details This subroutine handles deallocation of thermal analysis arrays including:
!!          - FTHE, FTHESKY: thermal force arrays
!!          - CONDN, CONDNSKY: thermal conductivity arrays
!!          - FTHESKYI, CONDNSKYI: thermal skyline arrays
subroutine resol_dealloc_thermal(glob_therm, wibem, wrbem, fthe, fthesky, &
                                  ftheskyi, condn, condnsky, condnskyi)
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
  real(kind=wp), allocatable, intent(inout) :: wibem(:)    !< Work array BEM integer
  real(kind=wp), allocatable, intent(inout) :: wrbem(:)    !< Work array BEM real
  real(kind=wp), allocatable, intent(inout) :: fthe(:)     !< Thermal force array
  real(kind=wp), allocatable, intent(inout) :: fthesky(:)  !< Thermal force skyline
  real(kind=wp), allocatable, intent(inout) :: ftheskyi(:) !< Implicit thermal force skyline
  real(kind=wp), allocatable, intent(inout) :: condn(:)    !< Thermal conductivity
  real(kind=wp), allocatable, intent(inout) :: condnsky(:) !< Conductivity skyline
  real(kind=wp), allocatable, intent(inout) :: condnskyi(:)!< Implicit conductivity skyline

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
  
  deallocate(wibem, wrbem)
  
  if (glob_therm%itherm_fe > 0) then
    deallocate(fthe, fthesky)
  endif
  
  if (glob_therm%intheat > 0) then
    deallocate(ftheskyi)
  endif
  
  if (glob_therm%nodadt_therm > 0) then
    deallocate(condn, condnsky)
  endif
  
  if (glob_therm%nodadt_therm > 0 .and. glob_therm%intheat > 0) then
    deallocate(condnskyi)
  endif

! ----------------------------------------------------------------------------------------------------------------------
end subroutine resol_dealloc_thermal

!! \brief Deallocate XFEM composite ply arrays
!! \details This subroutine handles deallocation of PLY and PLYSKY arrays for
!!          XFEM composite shell elements
subroutine resol_dealloc_plyxfem(iplyxfem, intplyxfem, nplymax, ply, plysky, plyskyi)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
  use plyxfem_mod, only : ply_type, plysky_type, plyskyi_type
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  integer, intent(in) :: iplyxfem                           !< PLY XFEM flag
  integer, intent(in) :: intplyxfem                         !< Interface PLY XFEM flag
  integer, intent(in) :: nplymax                            !< Maximum number of plies
  type(ply_type), allocatable, intent(inout) :: ply(:)     !< PLY array
  type(plysky_type), allocatable, intent(inout) :: plysky(:) !< PLY skyline array
  type(plyskyi_type), allocatable, intent(inout) :: plyskyi  !< PLY implicit skyline
  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
  integer :: i

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
  
  if (iplyxfem > 0) then
    do i = 1, nplymax
      deallocate(ply(i)%a)
      deallocate(ply(i)%v)
      deallocate(ply(i)%u)
      deallocate(plysky(i)%fsky)
    enddo
  endif
  
  if (intplyxfem > 0) then
    deallocate(plyskyi%fskyi)
  endif
  
  deallocate(ply, plysky)

! ----------------------------------------------------------------------------------------------------------------------
end subroutine resol_dealloc_plyxfem

end module resol_dealloc_mod
