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
module resol_timestep_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Calculate timestep based on geometry properties
!! \details Computes the minimum timestep considering geometry-based stability constraints
  subroutine resol_calc_geo_timestep(numgeo, nodadt, geo, dtfac1, dt2t, neltst, ityptst)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
    use constant_mod, only : zero
    use precision_mod, only : wp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    integer, intent(in) :: numgeo                   !< Number of geometries
    integer, intent(in) :: nodadt                   !< Nodal adaptive time step flag
    real(kind=wp), intent(in) :: geo(:,:)           !< Geometry properties array
    real(kind=wp), intent(in) :: dtfac1(3)          !< Time step scale factors
    real(kind=wp), intent(inout) :: dt2t            !< Current timestep
    integer, intent(out) :: neltst                  !< Element controlling timestep
    integer, intent(out) :: ityptst                 !< Element type controlling timestep
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
    integer :: i ! Loop counter for geometries
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------

    ! Calculate timestep based on geometry properties if applicable
    if (numgeo > 0 .and. nodadt == 0) then
      do i = 1, numgeo
        if (geo(5,i) > zero .and. dtfac1(3) * geo(5,i) < dt2t) then
          dt2t = dtfac1(3) * geo(5,i)
          neltst = 0
          ityptst = 3
        endif
      end do
    endif

! ----------------------------------------------------------------------------------------------------------------------
  end subroutine resol_calc_geo_timestep

end module resol_timestep_mod
