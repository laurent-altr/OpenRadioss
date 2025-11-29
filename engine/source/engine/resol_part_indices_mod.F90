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
module resol_part_indices_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Calculate part array indices for element types
!! \details Computes the starting indices for different element types in the IPART array
!!          This calculation is performed multiple times during the solve and is now centralized
  subroutine resol_calc_part_indices(lipart1, npart, nthpart, numels, numelq, numelc, &
                                      numelt, numelp, numelr, numeltg, numelx, numsph, numelig3d, &
                                      k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    integer, intent(in) :: lipart1                  !< Size parameter for IPART array
    integer, intent(in) :: npart                    !< Number of parts
    integer, intent(in) :: nthpart                  !< Number of thermal parts
    integer, intent(in) :: numels                   !< Number of solid elements
    integer, intent(in) :: numelq                   !< Number of quad shell elements
    integer, intent(in) :: numelc                   !< Number of shell elements (4-node)
    integer, intent(in) :: numelt                   !< Number of truss elements
    integer, intent(in) :: numelp                   !< Number of beam elements
    integer, intent(in) :: numelr                   !< Number of spring elements
    integer, intent(in) :: numeltg                  !< Number of triangular shell elements
    integer, intent(in) :: numelx                   !< Number of XFEM elements
    integer, intent(in) :: numsph                   !< Number of SPH particles
    integer, intent(in) :: numelig3d                !< Number of 3D isogeometric elements
    integer, intent(out) :: k1                      !< Index for base offset
    integer, intent(out) :: k2                      !< Index after solid elements
    integer, intent(out) :: k3                      !< Index after quad elements
    integer, intent(out) :: k4                      !< Index after shell elements
    integer, intent(out) :: k5                      !< Index after truss elements
    integer, intent(out) :: k6                      !< Index after beam elements
    integer, intent(out) :: k7                      !< Index after spring elements
    integer, intent(out) :: k8                      !< Index after spring elements (same as k7)
    integer, intent(out) :: k9                      !< Index after triangular shell elements
    integer, intent(out) :: k10                     !< Index after XFEM elements
    integer, intent(out) :: k11                     !< Index after SPH particles
    integer, intent(out) :: k12                     !< Index after 3D isogeometric elements
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------

    ! Calculate cumulative indices for element types in IPART array
    k1 = 1 + lipart1 * (npart + nthpart) + 2 * 9 * (npart + nthpart)
    k2 = k1 + numels
    k3 = k2 + numelq
    k4 = k3 + numelc
    k5 = k4 + numelt
    k6 = k5 + numelp
    k7 = k6 + numelr
    k8 = k7
    k9 = k8 + numeltg
    k10 = k9 + numelx
    k11 = k10 + numsph
    k12 = k11 + numelig3d

! ----------------------------------------------------------------------------------------------------------------------
  end subroutine resol_calc_part_indices

end module resol_part_indices_mod
