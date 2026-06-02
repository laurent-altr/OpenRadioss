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
          elements, shell_list, list_size, old_numnod, nthread, ispmd)
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
          integer :: i, j, k
          integer :: shell_id
          real(kind=wp), parameter   :: ZERO = 0._wp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          !Nodes 1682 - 1728
          ! Step 1 — Early exit if the parent node has no non-local DOFs
          nl_idx = nloc_dmg%idxi(old_local_id)
          if (nl_idx == 0) return

          ! Step 2 — DOF layout of the parent node
          old_pos = nloc_dmg%posi(nl_idx)
          nddl    = nloc_dmg%posi(nl_idx + 1) - old_pos
          new_pos = nloc_dmg%l_nloc + 1

          ! Step 3 — Extend IDXI (size old_numnod → new_local_id)
          call extend_array(nloc_dmg%idxi, old_numnod, new_local_id)
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

          ! Debug: print VNL/DNL/UNL of parent before reset so the split impact is visible
!          write(6,'(a,i8,a,i8)') ' SPLIT_NLOC parent_local=', old_local_id, &
!            ' new_local=', new_local_id
!          write(6,'(a,i4,a)') '   nddl=', nddl, ' DOFs:'
          do i = 1, nddl
            write(6,'(a,i3,4(a,1pe12.4))') '   dof=', i, &
              '  UNL=', nloc_dmg%unl    (old_pos+i-1), &
              '  VNL=', nloc_dmg%vnl    (old_pos+i-1), &
              '  VNL_OLD=', nloc_dmg%vnl_old(old_pos+i-1), &
              '  DNL=', nloc_dmg%dnl    (old_pos+i-1)
          end do

          nloc_dmg%unl    (new_pos:new_pos+nddl-1) = nloc_dmg%unl    (old_pos:old_pos+nddl-1)
          nloc_dmg%vnl    (new_pos:new_pos+nddl-1) = ZERO
          nloc_dmg%vnl_old(new_pos:new_pos+nddl-1) = ZERO
          nloc_dmg%dnl    (new_pos:new_pos+nddl-1) = ZERO

          ! Also zero the parent's VNL: its pre-split value was assembled from all
          ! elements including the ones now detached, and is no longer valid.
          nloc_dmg%vnl    (old_pos:old_pos+nddl-1) = ZERO
          nloc_dmg%vnl_old(old_pos:old_pos+nddl-1) = ZERO

          ! Count corners reassigned to the new node — needed for ADDCNE/PROCNE/IADC update
          ! in Step 8 (PARITH/ON path).  This is separate from mass handling.
          n_contrib = 0
          if (allocated(nloc_dmg%addcne)) then
            do i = 1, list_size
              shell_id = shell_list(i)
              do j = 1, 4
                if (elements%shell%nodes(j, shell_id) == new_local_id) then
                  n_contrib = n_contrib + 1
                end if
              end do
            end do
          end if

          nloc_dmg%mass (new_pos:new_pos+nddl-1) = nloc_dmg%mass (old_pos:old_pos+nddl-1)
          nloc_dmg%mass0(new_pos:new_pos+nddl-1) = nloc_dmg%mass0(old_pos:old_pos+nddl-1)
          ! Parent mass stays unchanged: the 50/50 split that was here previously reduced
          ! each node's mass by half, lowering the non-local critical time step by sqrt(2)
          ! below the original value.  Since the mechanical DT was calibrated against the
          ! full MASS (via CSTA/CDAMP), halving the mass causes the mechanical DT to exceed
          ! the non-local critical DT → exponential VNL divergence (~×1.10/cycle observed).
          ! Giving both parent and new node the full original mass keeps omega_max = sqrt(K/M)
          ! at or below its pre-split value (K is roughly halved per node since elements split
          ! between them), so the stability condition DT < 2/omega_max is preserved.

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

          ! Zero parent's FNL and STIFNL.
          !
          ! FNL for the parent node was assembled during the FORINT loop of THIS cycle,
          ! BEFORE the split occurred.  It reflects contributions from ALL elements
          ! previously connected to the parent (including the ones now detached to the
          ! new node).  Retaining this stale FNL causes two compounding problems:
          !
          !  1. Mass amplification: after the 50/50 mass split MASS_parent = 0.5*M_orig,
          !     so NLOCAL_ACC computes ACC = FNL_full / (0.5*M) = 2 × original.
          !
          !  2. DT-step amplification: if a tiny time-step (caused by the preceding
          !     divergence) recovers sharply in the first cycle after the split, the
          !     large DT multiplies the already-doubled ACC in NLOCAL_VEL, driving VNL
          !     back to the pre-split magnitude (or beyond) in one step.
          !
          ! Zeroing FNL here ensures that NLOCAL_VEL computes VNL_parent += DT12 * 0 = 0
          ! for this cycle.  NLOCAL_VEL also resets FNL to zero itself, so the next
          ! FORINT cycle assembles cleanly from the post-split connectivity.
          if (allocated(nloc_dmg%fnl)) then
            nloc_dmg%fnl   (old_pos:old_pos+nddl-1, 1:fnl_ncol)    = ZERO
          end if
          if (allocated(nloc_dmg%stifnl)) then
            nloc_dmg%stifnl(old_pos:old_pos+nddl-1, 1:stifnl_ncol) = ZERO
          end if
          ! PARITH/ON: also zero the parent's FSKY / STSKY rows.
          ! nl_idx is still the parent's non-local rank; addcne has not yet been extended
          ! for the new node (Step 8b does that), so addcne(nl_idx:nl_idx+1) is still valid.
          if (allocated(nloc_dmg%fsky) .and. allocated(nloc_dmg%addcne)) then
            j = nloc_dmg%addcne(nl_idx)
            k = nloc_dmg%addcne(nl_idx + 1) - 1
            if (k >= j) nloc_dmg%fsky (j:k, :) = ZERO
          end if
          if (allocated(nloc_dmg%stsky) .and. allocated(nloc_dmg%addcne)) then
            j = nloc_dmg%addcne(nl_idx)
            k = nloc_dmg%addcne(nl_idx + 1) - 1
            if (k >= j) nloc_dmg%stsky(j:k, :) = ZERO
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

          ! 8b — Extend ADDCNE (CSR row offsets) by one entry
          call extend_array(nloc_dmg%addcne, new_nnod, new_nnod + 1)
          nloc_dmg%addcne(new_nnod + 1) = nloc_dmg%addcne(new_nnod) + n_contrib

          ! 8c — Extend PROCNE and append MPI rank for each new contribution.
          !      The detached shells all live on the local domain, so their rank in
          !      PROCNE is ispmd+1 (PROCNE uses 1-based domain ranks throughout).
          !      NOTE: CNE is always allocated at size 0 (it is a legacy array whose
          !      role is now fully covered by FSKY/STSKY); it is intentionally not
          !      extended here.
          old_lcne = nloc_dmg%lcne_nl
          if (n_contrib > 0) then
            call extend_array(nloc_dmg%procne, old_lcne, old_lcne + n_contrib)
            k = old_lcne + 1
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

          ! 8d — Extend FSKY and STSKY, zeroing new rows
          old_fsky_rows = nloc_dmg%addcne(new_nnod)     ! row count before new node's block
          new_fsky_rows = nloc_dmg%addcne(new_nnod + 1)
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

            call extend_array(nloc_dmg%fsky,  old_fsky_rows, fsky_ncol, &
              new_fsky_rows, fsky_ncol)
            call extend_array(nloc_dmg%stsky, old_fsky_rows, stsky_ncol, &
              new_fsky_rows, stsky_ncol)
            ! Zero the new node's rows: old_fsky_rows is ADDCNE(new_nnod) = the old phantom
            ! row that becomes the new node's FIRST CC row.  Rows old_fsky_rows+1..new_fsky_rows-1
            ! are freshly allocated by extend_array (uninitialised).  new_fsky_rows itself is
            ! the new phantom row and is never referenced by ASSPAR_SUB, so it need not be zeroed.
            nloc_dmg%fsky (old_fsky_rows:new_fsky_rows-1, 1:fsky_ncol) = ZERO
            nloc_dmg%stsky(old_fsky_rows:new_fsky_rows-1, 1:stsky_ncol) = ZERO
          end if

          ! 8e — Update IADC back-pointers for detached quad shells.
          !      The ordering must match the order used to fill PROCNE above.
          !      IADTG (triangle shells) and IADS (solids) are NOT updated because
          !      node splitting is currently restricted to quad shells only.  If a
          !      split node is a corner of a non-local triangle shell or solid element,
          !      those back-pointers will be stale.  Add equivalent loops here when
          !      support for those element types is added to the node-splitting path.
          ! Start at old_fsky_rows = ADDCNE(new_nnod): this is the first row in the new
          ! node's ADDCNE range (the repurposed old phantom row, now first valid CC row).
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

        end subroutine detach_node_nloc

      end module detach_node_nloc_mod
