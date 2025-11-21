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
module resol_buffers_init_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Initialize iteration buffers at the beginning of each cycle
!! \details Resets output and contact buffers to zero for the new time step
  subroutine resol_buffers_init(output, h3d_data, mcont2, numnod, ngdone, nsgdone)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
    use output_mod, only : output_
    use h3d_mod, only : h3d_database
    use constant_mod, only : zero
    use precision_mod, only : wp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    type(output_), intent(inout) :: output          !< Output data structure
    type(h3d_database), intent(in) :: h3d_data      !< H3D output database
    real(kind=wp), intent(inout) :: mcont2(3,*)     !< Contact moment buffer
    integer, intent(in) :: numnod                   !< Number of nodes
    integer, intent(out) :: ngdone                  !< Group processing flag
    integer, intent(out) :: nsgdone                 !< Subgroup processing flag
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
    integer :: i ! Loop counter
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------

    ! Initialize optional force buffer
    if (allocated(output%data%fopt)) then
      output%data%fopt = zero
    endif

    ! Initialize contact vector buffer
    if (allocated(output%data%vect_cont2)) then
      output%data%vect_cont2 = zero
    endif

    ! Initialize contact moment buffer if needed
    if (h3d_data%n_vect_cont2m == 1) then
      do i = 1, numnod
        mcont2(1,i) = zero
        mcont2(2,i) = zero
        mcont2(3,i) = zero
      end do
    endif

    ! Set processing flags
    ngdone = 1
    nsgdone = 1

! ----------------------------------------------------------------------------------------------------------------------
  end subroutine resol_buffers_init

end module resol_buffers_init_mod
