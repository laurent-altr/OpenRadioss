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
!||====================================================================
!||    detach_node_nloc_mod   ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||--- called by ------------------------------------------------------
!||    detach_node            ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod       ../common_source/modules/connectivity.F90
!||    extend_array_mod       ../common_source/tools/memory/extend_array.F90
!||    nlocal_reg_mod         ../common_source/modules/nlocal_reg_mod.F
!||    precision_mod          ../common_source/modules/precision_mod.F90
!||====================================================================
      module detach_node_nloc_mod
        implicit none
      contains

!! \brief Update the non-local damage structure NLOC_DMG after a node split.
!!
!! \details When a node (old_local_id) is split into two, a new node
!!          (new_local_id = old numnod + 1) is created and a set of shell
!!          elements is re-connected to it.  This subroutine extends all
!!          non-local arrays in NLOC_DMG to account for the new node:
!!            - node-index tables (INDX, POSI, IDXI)
!!            - DOF-space state vectors (MASS, MASS0, VNL, VNL_OLD, DNL, UNL,
!!              FNL, STIFNL)
!!            - skyline connectivity (ADDCNE, PROCNE, FSKY, STSKY, IADC) for
!!              the PARITH/ON path
!!
!!          Must be called BEFORE nodes%numnod is incremented (i.e. from inside
!!          detach_node, after detach_node_from_shells).
!!          CNE is always allocated at size 0 and is never updated here.
!||====================================================================
!||    detach_node_nloc          ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||--- called by ------------------------------------------------------
!||    detach_node               ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    connectivity_mod          ../common_source/modules/connectivity.F90
!||    extend_array_mod          ../common_source/tools/memory/extend_array.F90
!||    nlocal_reg_mod            ../common_source/modules/nlocal_reg_mod.F
!||    precision_mod             ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine detach_node_nloc(nloc_dmg, old_local_id, new_local_id, &
          elements, shell_list, list_size, old_numnod, nthread, ispmd, is_mirror, n_owner_contrib, n_ghost_contrib, node_uid)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use nlocal_reg_mod
          use connectivity_mod
          use extend_array_mod
          use precision_mod, only : wp
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nlocal_str_),   intent(inout) :: nloc_dmg      !< non-local damage structure
          integer,             intent(in)    :: old_local_id  !< local id of the node being split
          integer,             intent(in)    :: new_local_id  !< local id of the new node (= old numnod + 1)
          type(connectivity_), intent(in)    :: elements      !< element connectivity (already updated for shell_list)
          integer,             intent(in)    :: list_size     !< number of shells being detached
          integer,             intent(in)    :: shell_list(list_size) !< local ids of detached shells
          integer,             intent(in)    :: old_numnod    !< total node count before the split
          integer,             intent(in)    :: nthread       !< number of threads (second dim of FNL/STIFNL)
          integer,             intent(in)    :: ispmd         !< local MPI rank (0-based); PROCNE uses 1-based ranks
          logical,             intent(in), optional :: is_mirror  !< .true. when called from mirror_node_split
          integer,             intent(in), optional :: n_owner_contrib !< ghost rank: number of owner N' local corners (= N'_ghost remote slots needed)
          integer,             intent(in), optional :: n_ghost_contrib !< owner rank: number of ghost shells moving to N' (for f_detach correction)
          integer,             intent(in), optional :: node_uid       !< global UID of the split node (for diagnostics)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nl_idx          ! non-local rank of the parent node
          integer :: old_pos         ! first DOF position of parent in non-local vectors
          integer :: nddl            ! DOF count of the parent node
          integer :: new_pos         ! first DOF position of the new node
          integer :: new_nnod        ! NNOD after the new node is added
          integer :: new_l_nloc      ! L_NLOC after extension
          integer :: n_contrib       ! number of FSKY row contributions for the new node
          integer :: n_old_contrib   ! FSKY row contributions remaining on the old node
          integer :: old_lcne        ! LCNE_NL before extension
          integer :: old_fsky_rows   ! number of FSKY rows before extension
          integer :: new_fsky_rows   ! number of FSKY rows after extension
          integer :: fsky_ncol, stsky_ncol
          integer :: fnl_ncol, stifnl_ncol
          integer :: i, j, k, i_el
          integer :: shell_id
          integer :: n_remain        ! corner count remaining on parent after split
          integer :: n_total         ! total corner count before split (n_remain + n_contrib)
          integer :: numelc          ! total number of shell elements
          integer :: n_remote        ! remote PROCNE entries (PROCNE ≠ ispmd+1) counted for diagnostic
          integer :: ghost_proc              ! first remote PROCNE rank in PARTIAL path (= ghost_rank + 1)
          integer :: n_ghost_local_contrib   ! ghost path: corners of ghost-rank shells going to N'
          integer :: cc              ! loop counter over ADDCNE/IADC entries
          integer :: parent_start    ! first FSKY row of parent node's ADDCNE range
          integer :: parent_end      ! last  FSKY row of parent node's ADDCNE range
          integer :: parent_total    ! total entries in parent's ADDCNE (local + remote)
          integer :: new_start       ! first FSKY row of new node's ADDCNE range
          real(kind=wp) :: f_retain  ! fraction of element corners retained by parent
          real(kind=wp) :: f_detach  ! fraction of element corners detached to child
          real(kind=wp), parameter   :: ZERO = 0._wp
          logical,       parameter   :: debug_detach_monitor = .false.  !! set .true. to print pre-split state
          logical :: l_is_mirror              ! local copy of is_mirror flag
          integer :: n_owner_contrib_local   ! local copy of n_owner_contrib (0 if not present)
          integer :: owner_proc              ! PROCNE of ghost parent's remote entries (= owner_rank + 1)
          integer :: n_ghost_contrib_local   ! local copy of n_ghost_contrib (0 if not present)
          integer :: n_contrib_global        ! n_contrib + n_ghost_contrib (all shells moving to N')
          integer :: n_total_global          ! parent_total from ADDCNE (all shells around parent)
          integer :: node_uid_local          ! local copy of node_uid (-1 if not present)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          parent_start = 0
          parent_end   = 0
          !Nodes 1682 - 1728
          ! Step 1 — Extend IDXI to cover new_local_id regardless of whether the parent
          !          has non-local DOFs.  This must happen before the early-exit check so
          !          that idxi is always sized to at least new_local_id; if we returned
          !          early without extending, the next split would call
          !          extend_array(idxi, numnod+1, numnod+2) with oldsize > size(idxi),
          !          causing a heap-buffer-overflow in the copy inside extend_array.
          call extend_array(nloc_dmg%idxi, old_numnod, new_local_id)
          ! The new node inherits no non-local DOFs by default (entry = 0).
          nloc_dmg%idxi(new_local_id) = 0

          nl_idx = nloc_dmg%idxi(old_local_id)
          if (nl_idx == 0) return

          ! Step 2 — DOF layout of the parent node
          old_pos = nloc_dmg%posi(nl_idx)
          nddl    = nloc_dmg%posi(nl_idx + 1) - old_pos
          new_pos = nloc_dmg%l_nloc + 1

          ! Step 3 — Set IDXI for the new node (extend_array already called in Step 1)
          nloc_dmg%idxi(new_local_id) = nloc_dmg%nnod + 1

          ! Step 4 — Extend INDX (size NNOD → NNOD+1)
          call extend_array(nloc_dmg%indx, nloc_dmg%nnod, nloc_dmg%nnod + 1)
          nloc_dmg%indx(nloc_dmg%nnod + 1) = new_local_id

          ! Step 5 — Extend POSI (size NNOD+1 → NNOD+2)
          !          POSI is 1-based; new node DOFs occupy [new_pos … new_pos+nddl-1]
          call extend_array(nloc_dmg%posi, nloc_dmg%nnod + 1, nloc_dmg%nnod + 2)
          nloc_dmg%posi(nloc_dmg%nnod + 2) = new_pos + nddl

          ! Step 6 — Extend DOF-space vectors by nddl entries and copy parent values
          new_l_nloc = nloc_dmg%l_nloc + nddl

          call extend_array(nloc_dmg%mass,    nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%mass0,   nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%vnl,     nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%vnl_old, nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%dnl,     nloc_dmg%l_nloc, new_l_nloc)
          call extend_array(nloc_dmg%unl,     nloc_dmg%l_nloc, new_l_nloc)

          ! Copy the cumulated state to the new node; reset kinematic increments.
          !
          ! UNL is the accumulated non-local variable (damage level at time of split):
          ! both sides of the crack start with the same value — copy it.
          !
          ! VNL is the "velocity" of the auxiliary explicit problem.  It was built up
          ! from ALL elements attached to the parent node before the split.  After the
          ! split the new node is an independent topological entity; inheriting the
          ! parent's VNL would propagate a large DNL = DT2*VNL into the element-force
          ! loop of the next cycle (dplanl >> 0 → dlam_nl huge → negative thickness).
          ! The split is called AFTER FORINT but BEFORE NLOCAL_VEL/NLOCAL_INCR, so VNL
          ! of the new node is consumed by NLOCAL_INCR in the same cycle: zeroing it
          ! here ensures DNL_new ≈ 0 in the next cycle.
          !
          ! For the same reason the PARENT node's VNL is also zeroed: after the split
          ! its connectivity changes (some elements moved to the new node), but its
          ! pre-split VNL was built from ALL elements and is no longer valid.  Leaving
          ! it non-zero propagates the same large-DNL artefact into the elements that
          ! remain connected to the parent.
          !
          ! VNL_OLD is overwritten by NLOCAL_VEL (VNL_OLD := VNL) before it is next
          ! read, so its value at split time is irrelevant — zero for clarity.
          !
          ! DNL is overwritten by NLOCAL_INCR (DNL := DT2*VNL) before it is next read
          ! by the element-force loop, so its value at split time is also irrelevant.

          ! --- Counting block -------------------------------------------------------
          ! Count element corners: detached (→ new_local_id) and retained (→ old_local_id).
          ! The connectivity was already updated by detach_node_from_shells before this
          ! call, so scanning elements%shell%nodes gives the post-split distribution.
          ! n_contrib also serves the PARITH/ON skyline update in Step 8.
          numelc    = size(elements%shell%nodes, 2)
          n_contrib = 0
          do i = 1, list_size
            shell_id = shell_list(i)
            do j = 1, 4
              if (elements%shell%nodes(j, shell_id) == new_local_id) n_contrib = n_contrib + 1
            end do
          end do
          n_remain = 0
          do i_el = 1, numelc
            do j = 1, 4
              if (elements%shell%nodes(j, i_el) == old_local_id) n_remain = n_remain + 1
            end do
          end do
          n_total = n_remain + n_contrib
          ! Safety: degenerate case — no corners on either node; fall back to equal share.
          if (n_total == 0) then
            f_retain = 0.5_wp
            f_detach = 0.5_wp
          else
            f_retain = real(n_remain,  wp) / real(n_total, wp)
            f_detach = real(n_contrib, wp) / real(n_total, wp)
          end if

          ! On MPI runs (PARITH/ON): correct f_detach/f_retain using the global corner count from
          ! ADDCNE so the VNL/mass scaling matches the 1-MPI result.  parent_total from ADDCNE
          ! counts ALL corners (local + remote).  The correction fires whenever remote entries
          ! exist (parent_total > n_contrib + n_remain), regardless of how many go to N'.
          l_is_mirror = .false.
          if (present(is_mirror)) l_is_mirror = is_mirror
          n_ghost_contrib_local = 0
          if (present(n_ghost_contrib)) n_ghost_contrib_local = n_ghost_contrib
          n_owner_contrib_local = 0
          if (present(n_owner_contrib)) n_owner_contrib_local = n_owner_contrib
          ! Owner rank: use global total when remote entries exist in parent's ADDCNE range
          if (.not. l_is_mirror .and. allocated(nloc_dmg%addcne)) then
            n_total_global   = nloc_dmg%addcne(nl_idx + 1) - nloc_dmg%addcne(nl_idx)
            n_contrib_global = n_contrib + n_ghost_contrib_local
            if (n_total_global > n_contrib + n_remain) then
              ! Remote entries exist — use global total for correct f_detach/f_retain
              f_detach = real(n_contrib_global, wp) / real(n_total_global, wp)
              f_retain  = real(n_total_global - n_contrib_global, wp) / real(n_total_global, wp)
            end if
          end if
          ! Ghost/mirror rank: correct f_detach/f_retain using owner shell count
          ! n_owner_contrib_local = owner's local shells going to N' (= remote slots in N'_ghost)
          if (l_is_mirror .and. allocated(nloc_dmg%addcne)) then
            n_total_global   = nloc_dmg%addcne(nl_idx + 1) - nloc_dmg%addcne(nl_idx)
            n_contrib_global = n_contrib + n_owner_contrib_local
            if (n_total_global > n_contrib + n_remain) then
              f_detach = real(n_contrib_global, wp) / real(n_total_global, wp)
              f_retain  = real(n_total_global - n_contrib_global, wp) / real(n_total_global, wp)
            end if
          end if

          node_uid_local = -1
          if (present(node_uid)) node_uid_local = node_uid
          write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,f6.3,a,f6.3)') &
            '[SPLIT_VNL][rank ', ispmd, '] uid=', node_uid_local, ' old=', old_local_id, &
            ' new=', new_local_id, ' n_contrib=', n_contrib, ' n_remain=', n_remain, &
            ' n_ghost=', n_ghost_contrib_local, &
            ' f_retain=', f_retain, ' f_detach=', f_detach
          flush(6)

          ! Debug: print state before modification so the split impact is visible.
          if (debug_detach_monitor) then
            write(6,'(a,i8,a,i8)') ' SPLIT_NLOC parent_local=', old_local_id, &
              ' new_local=', new_local_id
            write(6,'(a,i4,a)') '   nddl=', nddl, ' DOFs:'
            do i = 1, nddl
              write(6,'(a,i3,4(a,1pe12.4))') '   dof=', i, &
                '  UNL=', nloc_dmg%unl    (old_pos+i-1), &
                '  VNL=', nloc_dmg%vnl    (old_pos+i-1), &
                '  VNL_OLD=', nloc_dmg%vnl_old(old_pos+i-1), &
                '  DNL=', nloc_dmg%dnl    (old_pos+i-1)
            end do
            write(6,'(a,f6.3,a,f6.3,a,i0,a,i0,a)') &
              '   f_retain=', f_retain, '  f_detach=', f_detach, &
              '  (n_remain=', n_remain, ', n_contrib=', n_contrib, ')'
          end if

          ! UNL: both sides of the crack start with the same value — copy it.
          nloc_dmg%unl    (new_pos:new_pos+nddl-1) = nloc_dmg%unl    (old_pos:old_pos+nddl-1)
          ! DNL is overwritten by NLOCAL_INCR (DNL := DT2*VNL) before it is next read.
          nloc_dmg%dnl    (new_pos:new_pos+nddl-1) = ZERO

          ! VNL/MASS/FNL for PARENT: leave unchanged.
          !
          ! MPI consistency: in PARITH/ON the ghost copy of the parent node on remote
          ! ranks has the same VNL/MASS as the owner and receives the same FNL via the
          ! FSKY exchange.  NLOCAL_ACC and NLOCAL_VEL therefore compute identical VNL
          ! updates on all ranks — no owner-only modification is needed.
          !
          ! Physical interpretation: the split takes effect for the NEXT cycle's FORINT
          ! (via IADC/ADDCNE updates below).  In the CURRENT cycle FNL was assembled from
          ! the pre-split connectivity and represents a valid force.  Using it unmodified
          ! lets VNL evolve continuously without an artificial discontinuity.
          !
          ! Child node N': inherit a proportional share of VNL/MASS so that the split
          ! preserves the amplitude of the non-local wave at the crack tip.
          nloc_dmg%vnl    (new_pos:new_pos+nddl-1) = nloc_dmg%vnl    (old_pos:old_pos+nddl-1) * f_detach
          nloc_dmg%vnl_old(new_pos:new_pos+nddl-1) = nloc_dmg%vnl_old(old_pos:old_pos+nddl-1) * f_detach

          ! Child node gets a proportional share of the non-local mass so that the total
          ! mass is approximately conserved (parent keeps its original mass).
          nloc_dmg%mass (new_pos:new_pos+nddl-1) = nloc_dmg%mass (old_pos:old_pos+nddl-1) * f_detach
          nloc_dmg%mass0(new_pos:new_pos+nddl-1) = nloc_dmg%mass0(old_pos:old_pos+nddl-1) * f_detach

          ! Extend multithreaded accumulators; use actual allocated column counts
          ! (can be 1 in PARITH/ON, nthread in PARITH/OFF).
          if (allocated(nloc_dmg%fnl)) then
            fnl_ncol = size(nloc_dmg%fnl, 2)
          else
            fnl_ncol = nthread
          end if
          if (allocated(nloc_dmg%stifnl)) then
            stifnl_ncol = size(nloc_dmg%stifnl, 2)
          else
            stifnl_ncol = nthread
          end if

          call extend_array(nloc_dmg%fnl,    nloc_dmg%l_nloc, fnl_ncol, new_l_nloc, fnl_ncol)
          call extend_array(nloc_dmg%stifnl, nloc_dmg%l_nloc, stifnl_ncol, new_l_nloc, stifnl_ncol)
          nloc_dmg%fnl   (new_pos:new_pos+nddl-1, 1:fnl_ncol) = ZERO
          nloc_dmg%stifnl(new_pos:new_pos+nddl-1, 1:stifnl_ncol) = ZERO

          ! Step 7 — Commit scalar counters
          nloc_dmg%nnod   = nloc_dmg%nnod + 1
          nloc_dmg%l_nloc = new_l_nloc
          new_nnod        = nloc_dmg%nnod

          ! Step 8 — Skyline update for PARITH/ON
          !          (only when FSKY/ADDCNE/PROCNE are allocated, i.e. PARITH/ON path)
          ! (l_is_mirror already set above before f_detach correction block)

          if (.not. allocated(nloc_dmg%addcne)) return

          ! Warn when solid or triangle-shell non-local elements are present: their
          ! IADS/IADTG back-pointers are NOT updated here.  Node splitting is currently
          ! restricted to quad-shell meshes; adding support for those types requires
          ! equivalent loops below for IADS (8 corners) and IADTG (3 corners).
          if (nloc_dmg%numels_nl > 0 .or. nloc_dmg%numeltg_nl > 0) then
            write(6,'(a)') &
              ' ** WARNING detach_node_nloc: non-local solid/triangle-shell elements detected.'
            write(6,'(a)') &
              '    IADS/IADTG back-pointers are NOT updated after node split.'
            write(6,'(a)') &
              '    Non-local assembly may be incorrect for those element types.'
          end if

          ! Steps 8b-8f: skyline update strategy depends on whether parent is
          ! globally dead (all shells everywhere go to N') or partially alive.

          parent_start = nloc_dmg%addcne(nl_idx)
          parent_end   = nloc_dmg%addcne(nl_idx + 1) - 1
          parent_total = parent_end - parent_start + 1

          ! 8b — Extend ADDCNE (CSR row offsets) by one entry for the new node.
          call extend_array(nloc_dmg%addcne, new_nnod, new_nnod + 1)
          new_start = nloc_dmg%addcne(new_nnod)   ! old phantom becomes N'_3's first FSKY row

          if (l_is_mirror) then

            ! Ghost node: give N'_ghost n_owner_contrib_local REMOTE entries (PROCNE=owner_proc).
            ! If n_owner_contrib_local == parent_n_remote (owner used INHERIT_CLEAR,
            ! n_remain_owner==0), ghost parent's remote entries are also cleared so there
            ! is no dangling IRECSP in SPMD_SUB_BOUNDARIES.
            ! Ghost parent's local entries are compacted in-place because ordering is
            ! arbitrary (local and remote entries may be interleaved).
            n_owner_contrib_local = 0
            if (present(n_owner_contrib)) n_owner_contrib_local = n_owner_contrib

            ! Full scan: compute owner_proc and n_remote in a single pass (no early exit).
            owner_proc = 0
            n_remote = 0
            do cc = parent_start, parent_end
              if (nloc_dmg%procne(cc) /= ispmd + 1) then
                if (owner_proc == 0) owner_proc = nloc_dmg%procne(cc)
                n_remote = n_remote + 1
              end if
            end do

            ! If ghost parent has no remote entries, no OTG exchange exists; force 0.
            if (owner_proc == 0) n_owner_contrib_local = 0

            ! Determine fsky/stsky column counts (needed for compaction and N' extension).
            if (allocated(nloc_dmg%fsky)) then
              fsky_ncol = size(nloc_dmg%fsky, 2)
            else
              fsky_ncol = nloc_dmg%nddmax
            end if
            if (allocated(nloc_dmg%stsky)) then
              stsky_ncol = size(nloc_dmg%stsky, 2)
            else
              stsky_ncol = nloc_dmg%nddmax
            end if

            ! When all owner shells moved to N' (n_owner_contrib_local == n_remote > 0),
            ! the owner cleared its parent's ADDCNE (INHERIT_CLEAR).  Mirror that on the
            ! ghost side: compact ghost parent's ADDCNE to contain only LOCAL entries.
            ! Entries may be in arbitrary order; compact in-place, update IADC for moves.
            if (n_owner_contrib_local == n_remote .and. n_remote > 0) then
              k = parent_start
              do cc = parent_start, parent_end
                if (nloc_dmg%procne(cc) == ispmd + 1) then  ! local entry — keep it
                  if (k /= cc) then
                    nloc_dmg%procne(k) = nloc_dmg%procne(cc)
                    nloc_dmg%fsky (k, 1:fsky_ncol)  = nloc_dmg%fsky (cc, 1:fsky_ncol)
                    nloc_dmg%stsky(k, 1:stsky_ncol) = nloc_dmg%stsky(cc, 1:stsky_ncol)
                    do i_el = 1, numelc
                      do j = 1, 4
                        if (nloc_dmg%iadc(j, i_el) == cc) nloc_dmg%iadc(j, i_el) = k
                      end do
                    end do
                  end if
                  k = k + 1
                end if
                ! Remote entries are dropped: overwritten by subsequent local entries or abandoned.
              end do
              nloc_dmg%addcne(nl_idx + 1) = k
            end if

            ! Extend ADDCNE for N'_ghost with n_owner_contrib_local REMOTE entries.
            nloc_dmg%addcne(new_nnod + 1) = new_start + n_owner_contrib_local

            old_lcne = nloc_dmg%lcne_nl
            if (n_owner_contrib_local > 0) then
              call extend_array(nloc_dmg%procne, old_lcne, old_lcne + n_owner_contrib_local)
              do k = new_start, new_start + n_owner_contrib_local - 1
                nloc_dmg%procne(k) = owner_proc
              end do
              nloc_dmg%lcne_nl = old_lcne + n_owner_contrib_local
              old_fsky_rows = nloc_dmg%addcne(new_nnod)
              new_fsky_rows = nloc_dmg%addcne(new_nnod + 1)
              call extend_array(nloc_dmg%fsky,  old_fsky_rows, fsky_ncol, &
                new_fsky_rows, fsky_ncol)
              call extend_array(nloc_dmg%stsky, old_fsky_rows, stsky_ncol, &
                new_fsky_rows, stsky_ncol)
              nloc_dmg%fsky (old_fsky_rows:new_fsky_rows-1, 1:fsky_ncol)  = ZERO
              nloc_dmg%stsky(old_fsky_rows:new_fsky_rows-1, 1:stsky_ncol) = ZERO
            end if

            ! Diagnostic.
            write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)') &
              '[NLOC][rank ', ispmd, '] GHOST_PARTIAL old=', old_local_id, &
              ' new=', new_local_id, ' parent_total=', parent_total, &
              ' parent_n_remote=', n_remote, ' N_prime_remote=', n_owner_contrib_local, &
              ' owner_proc=', owner_proc
            flush(6)
            ! Dump ADDCNE + PROCNE for parent and N' on this ghost rank.
            write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0)') &
              '[NLOC_ROWS][rank ', ispmd, '] GHOST parent nl_idx=', nl_idx, &
              ' rows=', nloc_dmg%addcne(nl_idx+1)-nloc_dmg%addcne(nl_idx), &
              ' N_prime new_nnod=', new_nnod, &
              ' rows=', nloc_dmg%addcne(new_nnod+1)-nloc_dmg%addcne(new_nnod)
            block
              integer :: cc_
              do cc_ = nloc_dmg%addcne(new_nnod)+1, nloc_dmg%addcne(new_nnod+1)
                write(6,'(a,i0,a,i0,a,i0)') &
                  '[NLOC_PROCNE][rank ', ispmd, '] N_prime row ', cc_, &
                  ' proc=', nloc_dmg%procne(cc_)
              end do
            end block
            flush(6)

            ! 8e_ghost — Add LOCAL entries to ghost N' for ghost rank's own shells
            !            going to N', and update IADC so those shells fill ghost N's FSKY.
            !            Without this, ghost rank's local shells' contributions to N' are
            !            never routed: IADC still points at ghost_parent's rows (which may
            !            be dead), and owner N' has no REMOTE receive slot for them.
            if (list_size > 0) then
              n_ghost_local_contrib = 0
              do i = 1, list_size
                shell_id = shell_list(i)
                do j = 1, 4
                  if (elements%shell%nodes(j, shell_id) == new_local_id) then
                    n_ghost_local_contrib = n_ghost_local_contrib + 1
                  end if
                end do
              end do

              if (n_ghost_local_contrib > 0) then
                nloc_dmg%addcne(new_nnod + 1) = &
                  nloc_dmg%addcne(new_nnod + 1) + n_ghost_local_contrib

                old_lcne = nloc_dmg%lcne_nl
                call extend_array(nloc_dmg%procne, old_lcne, old_lcne + n_ghost_local_contrib)
                k = old_lcne + 1
                do i = 1, list_size
                  shell_id = shell_list(i)
                  do j = 1, 4
                    if (elements%shell%nodes(j, shell_id) == new_local_id) then
                      nloc_dmg%procne(k) = ispmd + 1  ! local rank (1-based)
                      k = k + 1
                    end if
                  end do
                end do
                nloc_dmg%lcne_nl = old_lcne + n_ghost_local_contrib

                old_fsky_rows = old_lcne + 1
                new_fsky_rows = nloc_dmg%lcne_nl + 1
                call extend_array(nloc_dmg%fsky,  old_fsky_rows, fsky_ncol, &
                  new_fsky_rows, fsky_ncol)
                call extend_array(nloc_dmg%stsky, old_fsky_rows, stsky_ncol, &
                  new_fsky_rows, stsky_ncol)
                nloc_dmg%fsky (old_fsky_rows:new_fsky_rows-1, 1:fsky_ncol)  = ZERO
                nloc_dmg%stsky(old_fsky_rows:new_fsky_rows-1, 1:stsky_ncol) = ZERO

                ! Update IADC: ghost shells going to N' now point to ghost N's LOCAL rows.
                k = old_fsky_rows  ! first LOCAL row in ghost N' = new_start + n_owner_contrib_local
                do i = 1, list_size
                  shell_id = shell_list(i)
                  do j = 1, 4
                    if (elements%shell%nodes(j, shell_id) == new_local_id) then
                      nloc_dmg%iadc(j, shell_id) = k
                      k = k + 1
                    end if
                  end do
                end do

                ! INHERIT_CLEAR: owner cleared parent's ADDCNE; ghost must mirror it.
                ! Ghost parent's compacted LOCAL entries are now routed to ghost N'
                ! via the IADC redirect above; clear ghost parent's ADDCNE so that
                ! SPMD_SUB_BOUNDARIES sees 0 entries (matching owner's cleared parent).
                if (n_owner_contrib_local == n_remote .and. n_remote > 0) then
                  nloc_dmg%addcne(nl_idx + 1) = nloc_dmg%addcne(nl_idx)
                end if
              end if
            end if

          else

            ! Owner split: compute n_remote (count of PROCNE entries that belong to
            ! other ranks) now that parent_start/parent_end are correctly set.
            n_remote = 0
            do cc = parent_start, parent_end
              if (nloc_dmg%procne(cc) /= ispmd + 1) n_remote = n_remote + 1
            end do

            ! INHERIT_CLEAR: n_remain==0 AND n_ghost_contrib==n_remote (all global
            !   shells go to N'; parent is globally dead).  N'_3 inherits all of
            !   parent's ADDCNE; parent's ADDCNE is cleared.
            ! PARTIAL: n_remain>0 OR n_ghost_contrib<n_remote (parent still has
            !   retained shells on other ranks).  N'_3 gets only n_contrib LOCAL
            !   entries; parent keeps its ADDCNE intact.  This matches 1-MPI behavior
            !   where parent retains its full ADDCNE (including stale detached-shell
            !   rows) so that FNL[parent] is non-zero at the split cycle.
            if (n_remain == 0 .and. n_ghost_contrib_local == n_remote) then

              ! INHERIT_CLEAR: N'_3 inherits the complete ADDCNE range from parent.
              nloc_dmg%addcne(new_nnod + 1) = new_start + parent_total

              old_lcne = nloc_dmg%lcne_nl
              if (parent_total > 0) then
                call extend_array(nloc_dmg%procne, old_lcne, old_lcne + parent_total)
                nloc_dmg%procne(new_start:new_start + parent_total - 1) = &
                  nloc_dmg%procne(parent_start:parent_end)
                nloc_dmg%lcne_nl = old_lcne + parent_total
              end if

              old_fsky_rows = nloc_dmg%addcne(new_nnod)
              new_fsky_rows = nloc_dmg%addcne(new_nnod + 1)
              if (parent_total > 0) then
                if (allocated(nloc_dmg%fsky)) then
                  fsky_ncol = size(nloc_dmg%fsky, 2)
                else
                  fsky_ncol = nloc_dmg%nddmax
                end if
                if (allocated(nloc_dmg%stsky)) then
                  stsky_ncol = size(nloc_dmg%stsky, 2)
                else
                  stsky_ncol = nloc_dmg%nddmax
                end if
                call extend_array(nloc_dmg%fsky,  old_fsky_rows, fsky_ncol, &
                  new_fsky_rows, fsky_ncol)
                call extend_array(nloc_dmg%stsky, old_fsky_rows, stsky_ncol, &
                  new_fsky_rows, stsky_ncol)
                nloc_dmg%fsky (old_fsky_rows:new_fsky_rows-1, 1:fsky_ncol)  = ZERO
                nloc_dmg%stsky(old_fsky_rows:new_fsky_rows-1, 1:stsky_ncol) = ZERO
              end if

              ! Redirect ALL IADC in [parent_start, parent_end] to N'_3's range.
              ! Offset is preserved: cc → new_start + (cc - parent_start).
              do i_el = 1, numelc
                do j = 1, 4
                  cc = nloc_dmg%iadc(j, i_el)
                  if (cc >= parent_start .and. cc <= parent_end) then
                    nloc_dmg%iadc(j, i_el) = new_start + (cc - parent_start)
                  end if
                end do
              end do

              ! Clear parent's ADDCNE: set end = start → 0 entries.
              ! Parent stays alive (IDXI unchanged) for other options (trusses, etc.).
              nloc_dmg%addcne(nl_idx + 1) = nloc_dmg%addcne(nl_idx)

              ! Diagnostic.
              write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)') &
                '[NLOC][rank ', ispmd, '] INHERIT_CLEAR old=', old_local_id, &
                ' new=', new_local_id, ' parent_total=', parent_total, &
                ' n_contrib=', n_contrib, ' parent_n_remote=', n_remote
              flush(6)

            else

              ! PARTIAL: n_remain > 0 — N'_3 gets only n_contrib LOCAL entries.
              ! Parent always keeps its ADDCNE intact and is never deactivated,
              ! so it can continue serving other options (trusses, springs, etc.).
              nloc_dmg%addcne(new_nnod + 1) = new_start + n_contrib

              ! 8c — Extend PROCNE and append local-domain rank for each new contribution.
              !      All detached shells live on this domain, so PROCNE = ispmd+1.
              !      CNE is always size 0 (legacy) and is intentionally not extended here.
              old_lcne = nloc_dmg%lcne_nl
              if (n_contrib > 0) then
                call extend_array(nloc_dmg%procne, old_lcne, old_lcne + n_contrib)
                k = new_start
                do i = 1, list_size
                  shell_id = shell_list(i)
                  do j = 1, 4
                    if (elements%shell%nodes(j, shell_id) == new_local_id) then
                      nloc_dmg%procne(k) = ispmd + 1  ! 1-based local domain rank
                      k = k + 1
                    end if
                  end do
                end do
                nloc_dmg%lcne_nl = old_lcne + n_contrib
              end if

              ! 8d — Extend FSKY and STSKY, zeroing new rows.
              old_fsky_rows = nloc_dmg%addcne(new_nnod)     ! old phantom = N'_3's first row
              new_fsky_rows = nloc_dmg%addcne(new_nnod + 1) ! = old_fsky_rows + n_contrib
              if (n_contrib > 0) then
                if (allocated(nloc_dmg%fsky)) then
                  fsky_ncol = size(nloc_dmg%fsky, 2)
                else
                  fsky_ncol = nloc_dmg%nddmax
                end if
                if (allocated(nloc_dmg%stsky)) then
                  stsky_ncol = size(nloc_dmg%stsky, 2)
                else
                  stsky_ncol = nloc_dmg%nddmax
                end if
                ! Defensive: verify old_fsky_rows matches actual allocation
                if (allocated(nloc_dmg%fsky) .and. &
                  old_fsky_rows /= size(nloc_dmg%fsky, 1)) then
                  write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0)') &
                    '[NLOC_BUG] rank=', ispmd, &
                    ' 8d fsky mismatch: old_fsky_rows=', old_fsky_rows, &
                    ' size(fsky,1)=', size(nloc_dmg%fsky, 1), &
                    ' new_nnod=', new_nnod, ' lcne_nl=', nloc_dmg%lcne_nl
                  flush(6)
                end if
                call extend_array(nloc_dmg%fsky,  old_fsky_rows, fsky_ncol, &
                  new_fsky_rows, fsky_ncol)
                call extend_array(nloc_dmg%stsky, old_fsky_rows, stsky_ncol, &
                  new_fsky_rows, stsky_ncol)
                nloc_dmg%fsky (old_fsky_rows:new_fsky_rows-1, 1:fsky_ncol)  = ZERO
                nloc_dmg%stsky(old_fsky_rows:new_fsky_rows-1, 1:stsky_ncol) = ZERO
              end if

              ! 8e — Update IADC for detached shells only (ordering matches PROCNE loop).
              !      IADTG / IADS not updated: node splitting is restricted to quad shells.
              !      If a split node is a corner of a non-local triangle shell or solid,
              !      add equivalent loops here for IADTG (3 corners) and IADS (8 corners).
              k = old_fsky_rows
              do i = 1, list_size
                shell_id = shell_list(i)
                do j = 1, 4
                  if (elements%shell%nodes(j, shell_id) == new_local_id) then
                    nloc_dmg%iadc(j, shell_id) = k
                    k = k + 1
                  end if
                end do
              end do

              ! 8f — PARTIAL + ghost shells going to N': add REMOTE entries to N' so that
              !      SPMD_EXCH can receive the ghost rank's FSKY contribution for N'.
              !      N' entries: n_contrib LOCAL (owner's shells) + n_ghost_contrib REMOTE (ghost's shells).
              if (n_ghost_contrib_local > 0) then
                ghost_proc = 0
                do cc = parent_start, parent_end
                  if (nloc_dmg%procne(cc) /= ispmd + 1) then
                    ghost_proc = nloc_dmg%procne(cc)
                    exit
                  end if
                end do

                if (ghost_proc > 0) then
                  old_lcne = nloc_dmg%lcne_nl
                  call extend_array(nloc_dmg%procne, old_lcne, &
                    old_lcne + n_ghost_contrib_local)
                  do k = old_lcne + 1, old_lcne + n_ghost_contrib_local
                    nloc_dmg%procne(k) = ghost_proc
                  end do
                  nloc_dmg%lcne_nl = old_lcne + n_ghost_contrib_local
                  nloc_dmg%addcne(new_nnod + 1) = &
                    nloc_dmg%addcne(new_nnod + 1) + n_ghost_contrib_local

                  old_fsky_rows = old_lcne + 1
                  new_fsky_rows = nloc_dmg%lcne_nl + 1
                  call extend_array(nloc_dmg%fsky,  old_fsky_rows, fsky_ncol, &
                    new_fsky_rows, fsky_ncol)
                  call extend_array(nloc_dmg%stsky, old_fsky_rows, stsky_ncol, &
                    new_fsky_rows, stsky_ncol)
                  nloc_dmg%fsky (old_fsky_rows:new_fsky_rows-1, 1:fsky_ncol)  = ZERO
                  nloc_dmg%stsky(old_fsky_rows:new_fsky_rows-1, 1:stsky_ncol) = ZERO
                end if
              end if

              ! 8g — Diagnostic: partial split stats.
              n_remote = 0
              do cc = parent_start, parent_end
                if (nloc_dmg%procne(cc) /= ispmd + 1) n_remote = n_remote + 1
              end do
              write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)') &
                '[NLOC][rank ', ispmd, '] PARTIAL old=', old_local_id, &
                ' new=', new_local_id, ' parent_total=', parent_total, &
                ' n_contrib=', n_contrib, ' n_remain=', n_remain, &
                ' parent_n_remote=', n_remote
              flush(6)
              ! Dump ADDCNE + PROCNE for parent and N' on this owner rank.
              write(6,'(a,i0,a,i0,a,i0,a,i0,a,i0)') &
                '[NLOC_ROWS][rank ', ispmd, '] OWNER parent nl_idx=', nl_idx, &
                ' rows=', nloc_dmg%addcne(nl_idx+1)-nloc_dmg%addcne(nl_idx), &
                ' N_prime new_nnod=', new_nnod, &
                ' rows=', nloc_dmg%addcne(new_nnod+1)-nloc_dmg%addcne(new_nnod)
              block
                integer :: cc_
                do cc_ = nloc_dmg%addcne(new_nnod)+1, nloc_dmg%addcne(new_nnod+1)
                  write(6,'(a,i0,a,i0,a,i0)') &
                    '[NLOC_PROCNE][rank ', ispmd, '] N_prime row ', cc_, &
                    ' proc=', nloc_dmg%procne(cc_)
                end do
                do cc_ = nloc_dmg%addcne(nl_idx)+1, nloc_dmg%addcne(nl_idx+1)
                  write(6,'(a,i0,a,i0,a,i0)') &
                    '[NLOC_PROCNE][rank ', ispmd, '] parent row ', cc_, &
                    ' proc=', nloc_dmg%procne(cc_)
                end do
              end block
              flush(6)

            end if  ! n_remain == 0

          end if  ! l_is_mirror

        end subroutine detach_node_nloc

      end module detach_node_nloc_mod
