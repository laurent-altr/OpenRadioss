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
module resol_alloc_loadp_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Allocate load pressure arrays
!! \details This routine allocates arrays for /LOAD/PRESSURE tables, interface areas and TH/SURF output channels
  subroutine resol_alloc_loadp(nloadp_hyd, nintloadp, nloadp_f, nloadp, ninter, &
                                numnod, nsurf, sizloadp, iloadp, nloadp_hyd_inter, &
                                ierror, s_loadpinter, npresload, th_surf_num_channel, &
                                loadp_hyd_inter, tagncont, loadp_tagdel, interfaces_intcarea, &
                                interfaces_intarean, th_surf_channels, th_surf_iok, &
                                th_surf_loadp_flag, th_surf_nsegloadpf, th_surf_nsegloadpb, &
                                th_surf_nsegloadp, pblast_nloadp_b)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
    use precision_mod, only : WP
    use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    integer, intent(in) :: nloadp_hyd            !< Number of hydro load pressure
    integer, intent(in) :: nintloadp             !< Number of interface load pressure
    integer, intent(in) :: nloadp_f              !< Number of load pressure F
    integer, intent(in) :: nloadp                !< Number of load pressure
    integer, intent(in) :: ninter                !< Number of interfaces
    integer, intent(in) :: numnod                !< Number of nodes
    integer, intent(in) :: nsurf                 !< Number of surfaces
    integer, intent(in) :: sizloadp              !< Size of load pressure data
    integer, intent(in) :: iloadp(*)             !< Load pressure data array
    integer, intent(out) :: nloadp_hyd_inter     !< Number of hydro load pressure interfaces
    integer, intent(out) :: ierror               !< Error flag
    integer, intent(out) :: s_loadpinter         !< Size of load pressure interface
    integer, intent(out) :: npresload            !< Number of pressure loads
    integer, intent(in) :: th_surf_num_channel   !< Number of TH surf channels
    integer, allocatable, intent(out) :: loadp_hyd_inter(:)      !< Load pressure hydro interface array
    integer, allocatable, intent(out) :: tagncont(:,:)           !< Tag node contact array
    integer, allocatable, intent(out) :: loadp_tagdel(:)         !< Load pressure tag delete array
    integer, intent(in) :: interfaces_intcarea                   !< Interface area flag
    real(kind=WP), allocatable, intent(out) :: interfaces_intarean(:) !< Interface area per node
    real(kind=WP), allocatable, intent(out) :: th_surf_channels(:,:)  !< TH surface channels array
    integer, intent(in) :: th_surf_iok           !< TH surf OK flag
    integer, intent(in) :: th_surf_loadp_flag    !< TH surf load pressure flag
    integer, intent(out) :: th_surf_nsegloadpf   !< Number of segments load pressure F
    integer, intent(out) :: th_surf_nsegloadpb   !< Number of segments load pressure B
    integer, intent(out) :: th_surf_nsegloadp    !< Number of segments load pressure
    integer, intent(in) :: pblast_nloadp_b       !< PBLAST number of load pressure B
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
    integer :: k
    integer :: ierror2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------

    ! Allocation of tables for /LOAD/PRESSURE
    nloadp_hyd_inter = 0
    ierror = 0
    if (nintloadp > 0) then
      allocate(loadp_hyd_inter(nloadp_hyd), stat=ierror2)
      ierror = ierror + ierror2
      do k = 1, nloadp_hyd
        if (iloadp(sizloadp*(k-1)+5) > 0) then
          nloadp_hyd_inter = nloadp_hyd_inter + 1
          loadp_hyd_inter(k) = nloadp_hyd_inter
        end if
      end do
    end if

    if (nloadp_hyd_inter > 0) then
      allocate(tagncont(nloadp_hyd_inter,numnod), stat=ierror2)
      ierror = ierror + ierror2
      if (ierror /= 0) then
        call ancmsg(msgid=158, anmode=aninfo, i1=ierror)
        call arret(2)
      end if
      tagncont = 0
    else
      allocate(tagncont(0,0))
    end if

    s_loadpinter = ninter*nloadp_hyd

    npresload = 0
    if (nloadp_hyd > 0) then
      do k = 1, nloadp_hyd
        npresload = npresload + iloadp(sizloadp*(k-1)+1)/4
      end do
      allocate(loadp_tagdel(npresload), stat=ierror2)
      loadp_tagdel(1:npresload) = 0
    else
      allocate(loadp_tagdel(0))
    end if

    if (interfaces_intcarea > 0) then
      allocate(interfaces_intarean(numnod))
    else
      allocate(interfaces_intarean(0))
    end if

    ! /TH/SURF output
    if (nsurf > 0) then
      allocate(th_surf_channels(th_surf_num_channel,nsurf))
      th_surf_channels(1:th_surf_num_channel,1:nsurf) = 0.0_WP
    else
      allocate(th_surf_channels(0,0))
    end if

    if (th_surf_iok > 0) then
      if (th_surf_loadp_flag > 0) then
        th_surf_nsegloadpf = 0
        do k = 1, nloadp_f
          th_surf_nsegloadpf = th_surf_nsegloadpf + iloadp(sizloadp*(k-1)+1)/4
        end do
        th_surf_nsegloadpb = 0
        do k = nloadp_f+1, pblast_nloadp_b
          th_surf_nsegloadpb = th_surf_nsegloadpb + iloadp(sizloadp*(k-1)+1)/4
        end do
        th_surf_nsegloadp = 0
        do k = nloadp_f+pblast_nloadp_b+1, nloadp
          th_surf_nsegloadp = th_surf_nsegloadp + iloadp(sizloadp*(k-1)+1)/4
        end do
      end if
    end if

! ----------------------------------------------------------------------------------------------------------------------
  end subroutine resol_alloc_loadp
end module resol_alloc_loadp_mod
