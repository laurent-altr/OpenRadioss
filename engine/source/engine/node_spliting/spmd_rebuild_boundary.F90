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
!||    spmd_rebuild_boundary_mod   ../engine/source/engine/node_spliting/spmd_rebuild_boundary.F90
!||--- called by ------------------------------------------------------
!||    resol                       ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    spmd_mod                    ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_rebuild_boundary_mod

        implicit none
        private
        public :: spmd_rebuild_boundary

      contains

!||====================================================================
!||    spmd_rebuild_boundary   ../engine/source/mpi/nodes/spmd_rebuild_boundary.F90
!||--- called by ------------------------------------------------------
!||    resol                   ../engine/source/engine/resol.F
!||====================================================================
!! \brief Rebuild BOUNDARY and BOUNDARY_ADD from scratch.
!!
!! \details
!! Uses two collective MPI operations:
!!
!!   Round 1 — MPI_Allgatherv
!!     Every rank broadcasts the global IDs of its ghost nodes (W=0).
!!     Each rank then finds which of its OWN nodes (W=1) appear in other
!!     ranks' ghost lists.  Those are the nodes it must send during force
!!     exchange: they go into send_buf, grouped by target rank.
!!
!!   Round 2 — MPI_Alltoallv
!!     Each rank sends send_buf to the relevant neighbours and receives
!!     the mirror list: nodes the neighbour owns that this rank ghosts.
!!     Together the two halves form the complete, symmetric exchange list.
!!
!! The resulting BOUNDARY stores LOCAL node indices (same indexing as
!! A, V, X, ...).  For rank J-1 (1-indexed J), BOUNDARY_ADD lays out:
!!   recv sub-block: BOUNDARY( BOUNDARY_ADD(1,J) : BOUNDARY_ADD(2,J)-1 )
!!     nodes owned by rank J-1 that this rank ghosts (receives)
!!   send sub-block: BOUNDARY( BOUNDARY_ADD(2,J) : BOUNDARY_ADD(1,J+1)-1 )
!!     nodes owned by this rank that rank J-1 ghosts (sends)
!!
!! \param[in]  numnod           local number of nodes on this MPI rank
!! \param[in]  nspmd            total number of MPI ranks
!! \param[in]  ispmd            this rank's 0-based MPI rank
!! \param[in]  main_proc(numnod) owning MPI rank (0-based) for each local node
!! \param[in]  itab(numnod)     global unique node ID for each local node
!! \param[out] boundary         flat list of local node indices grouped by rank;
!!                              (re)allocated here, caller is responsible for
!!                              deallocation if previously allocated
!! \param[out] boundary_add     CSR index array, shape (2, nspmd+1);
!!                              row 1: start of each domain's block;
!!                              row 2: start of each domain's send sub-block
!! \param[out] boundary_size    total number of entries in boundary

        subroutine spmd_rebuild_boundary(numnod, nspmd, ispmd, nodes)

          use spmd_mod,  only : spmd_allgatherv, spmd_allgather, spmd_allreduce, &
            spmd_alltoall, spmd_alltoallv, SPMD_MAX
          use nodal_arrays_mod

          implicit none

          ! ------------------------------------------------------------------ arguments
          integer, intent(in)  :: numnod
          integer, intent(in)  :: nspmd
          integer, intent(in)  :: ispmd                     ! 0-based MPI rank
!         integer, intent(in)  :: main_proc(numnod)         ! owning rank (0-based) per node
!         integer, intent(in)  :: itab(numnod)
          type(nodal_arrays_), intent(inout)  :: nodes
!         integer, allocatable, intent(out) :: boundary(:)
!         integer, allocatable, intent(out) :: boundary_add(:,:)
!         integer, intent(out) :: boundary_size

          ! ------------------------------------------------------------------ locals
          integer :: i, j, k, gid, lidx
          integer :: n_ghost_local, max_gid_local, max_gid, s_tot, r_tot, pos

          ! ghost list for this rank
          integer, allocatable :: ghost_gids(:)

          ! Allgatherv buffers (ghost lists from all ranks)
          integer, allocatable :: ghost_counts(:)   ! number of ghost nodes per rank
          integer, allocatable :: ghost_displs(:)   ! displacements (0-based, length nspmd+1)
          integer, allocatable :: all_ghost(:)      ! concatenated ghost global IDs

          ! global_id -> local_index reverse map (direct, sized to max global ID)
          integer, allocatable :: g2l(:)

          ! Alltoallv buffers
          integer, allocatable :: send_cnt(:)   ! nodes to send to each rank
          integer, allocatable :: send_dsp(:)   ! send buffer displacements (0-based, length nspmd+1)
          integer, allocatable :: send_buf(:)   ! global IDs to send, packed by rank

          integer, allocatable :: recv_cnt(:)   ! nodes to receive from each rank
          integer, allocatable :: recv_dsp(:)   ! recv buffer displacements (0-based, length nspmd+1)
          integer, allocatable :: recv_buf(:)   ! global IDs received, packed by rank

          if(nspmd > 1) then
            ! ================================================================
            ! Phase 1 — collect ghost global IDs on this rank
            ! ================================================================
! main_proc starts at 1 to nspmd, so subtract 1 to get 0-based rank
            n_ghost_local = 0
            do i = 1, numnod
              if (nodes%main_proc(i) -1 /= ispmd) n_ghost_local = n_ghost_local + 1
              if(nodes%itab(i) ==  13550 .and. nodes%main_proc(i)-1 /= ispmd) then
                write(6,*) "spmd_rebuild_boundary:  found ghost node with global id 13550 at local index ", i, nodes%MAIN_PROC(i)-1
                call flush(6)
              end if
            end do

            allocate(ghost_gids(max(1, n_ghost_local)))
            k = 0
            do i = 1, numnod
              if (nodes%main_proc(i) -1 /= ispmd) then
                k = k + 1
                ghost_gids(k) = nodes%itab(i)
                if(nodes%itab(i) ==  13550) then
                  write(6,*)  " added ghost node with uid 13550 at local index ", i, " to ghost_gids(", k, ")", nodes%MAIN_PROC(i)-1
                  call flush(6)
                end if
              end if
            end do

            ! ================================================================
            ! Phase 2 — Allgatherv: broadcast ghost lists to all ranks
            !
            ! After this, all_ghost(ghost_displs(J)+1 : ghost_displs(J+1))
            ! holds the ghost global IDs of rank J-1 (0-based).
            ! ghost_displs uses 0-based offsets as required by MPI.
            ! ================================================================
            allocate(ghost_counts(nspmd))
            allocate(ghost_displs(nspmd + 1))

            call spmd_allgather(n_ghost_local, ghost_counts, 1)

            ghost_displs(1) = 0
            do j = 1, nspmd
              ghost_displs(j + 1) = ghost_displs(j) + ghost_counts(j)
            end do

            allocate(all_ghost(max(1, ghost_displs(nspmd + 1))))

            ! spmd_allgatherv expects 0-based displacements; ghost_displs(1:nspmd)
            ! provides them (element 1 = displacement for rank 0 = 0, etc.)
            call spmd_allgatherv(ghost_gids, n_ghost_local, all_ghost, &
              ghost_counts, ghost_displs)

            ! ================================================================
            ! Phase 3 — build reverse map  global_id -> local_index
            !
            ! A direct array g2l is efficient because OpenRadioss global node
            ! IDs are contiguous starting from 1.  g2l(gid) = 0 means gid is
            ! not present on this rank.
            ! ================================================================
            max_gid_local = 0
            do i = 1, numnod
              if (nodes%itab(i) > max_gid_local) then
                max_gid_local = nodes%itab(i)
                if(nodes%itab(i) ==  13550) then
                  write(6,*) "spmd_rebuild_boundary: rank ", ispmd, " found local node with global id 13550 at local index ", i
                  call flush(6)
                end if
              end if
            end do
            call spmd_allreduce(max_gid_local, max_gid, 1, SPMD_MAX)

            allocate(g2l(max_gid))
            g2l(:) = 0
            do i = 1, numnod
              g2l(nodes%itab(i)) = i
              if(nodes%itab(i) ==  13550) then
                write(6,*) "spmd_rebuild_boundary: rank ", ispmd, " set g2l(13550) = ", i
                call flush(6)
              end if
            end do

            ! ================================================================
            ! Phase 4 — for each rank J, identify nodes I own that J ghosts
            !           → pack their global IDs into send_buf, grouped by J
            !
            ! send_dsp(J) = 0-based start of rank J-1's segment in send_buf.
            ! Two passes: first count, then fill.
            ! ================================================================
            allocate(send_cnt(nspmd), send_dsp(nspmd + 1))
            send_cnt(:) = 0

            do j = 1, nspmd
              if (j - 1 == ispmd) cycle                           ! skip self
              do k = ghost_displs(j) + 1, ghost_displs(j + 1)    ! rank J-1's ghost list
                gid  = all_ghost(k)
                lidx = g2l(gid)
                if (lidx > 0) then                                ! guard: gid must be local
                  if (nodes%main_proc(lidx)-1 == ispmd) &                 ! I own this node
                    send_cnt(j) = send_cnt(j) + 1
                end if
              end do
            end do

            send_dsp(1) = 0
            do j = 1, nspmd
              send_dsp(j + 1) = send_dsp(j) + send_cnt(j)
            end do
            s_tot = send_dsp(nspmd + 1)

            allocate(send_buf(max(1, s_tot)))

            do j = 1, nspmd
              if (j - 1 == ispmd) cycle
              pos = send_dsp(j)
              do k = ghost_displs(j) + 1, ghost_displs(j + 1)
                gid  = all_ghost(k)
                lidx = g2l(gid)
                if (lidx > 0) then                                ! guard: gid must be local
                  if (nodes%main_proc(lidx)-1 == ispmd) then              ! I own this node
                    pos = pos + 1
                    send_buf(pos) = gid
                    if(gid ==  13550) then
                      write(6,*) "spmd_rebuild_boundary:added at  index ", lidx, " to send_buf(", pos, ") for rank ", j-1
                      call flush(6)
                    end if
                  end if
                end if
              end do
            end do

            ! ================================================================
            ! Phase 5 — Alltoallv: exchange boundary node lists
            !
            ! Rank J receives from rank A the list of nodes A owns that J
            ! ghosts.  Equivalently, rank A receives from each J the list of
            ! nodes J owns that A ghosts.  Together, send_buf and recv_buf
            ! cover both directions of the exchange symmetrically.
            ! ================================================================
            allocate(recv_cnt(nspmd), recv_dsp(nspmd + 1))

            ! Negotiate sizes first (Alltoall of counts, size-1 messages)
            call spmd_alltoall(send_cnt, 1, recv_cnt, 1)

            recv_dsp(1) = 0
            do j = 1, nspmd
              recv_dsp(j + 1) = recv_dsp(j) + recv_cnt(j)
            end do
            r_tot = recv_dsp(nspmd + 1)

            allocate(recv_buf(max(1, r_tot)))

            ! send_dsp(1:nspmd) and recv_dsp(1:nspmd) are the 0-based MPI
            ! displacements for ranks 0..nspmd-1 respectively.
            call spmd_alltoallv(send_buf, send_cnt, send_dsp(1:nspmd), &
              recv_buf, recv_cnt, recv_dsp(1:nspmd))

            ! ================================================================
            ! Phase 6 — assemble BOUNDARY and BOUNDARY_ADD
            !
            ! For each domain J (1-indexed, domain J-1 in 0-based MPI):
            !   recv sub-block: BOUNDARY( BOUNDARY_ADD(1,J) : BOUNDARY_ADD(2,J)-1 )
            !     local indices of nodes owned by rank J-1 that this rank ghosts
            !   send sub-block: BOUNDARY( BOUNDARY_ADD(2,J) : BOUNDARY_ADD(1,J+1)-1 )
            !     local indices of nodes owned by this rank that rank J-1 ghosts
            ! This layout matches what IAD_ELEM(2,I) consumers (SPMD_SD_XV etc.) expect.
            ! ================================================================
            if (allocated(nodes%boundary_add)) deallocate(nodes%boundary_add)
            if (allocated(nodes%boundary))     deallocate(nodes%boundary)

            allocate(nodes%boundary_add(2, nspmd + 1))
            nodes%boundary_add(1, 1) = 1
            do j = 1, nspmd
              nodes%boundary_add(2, j)     = nodes%boundary_add(1, j) + recv_cnt(j)
              nodes%boundary_add(1, j + 1) = nodes%boundary_add(2, j) + send_cnt(j)
            end do
            nodes%boundary_add(2, nspmd + 1) = nodes%boundary_add(1, nspmd + 1)  ! sentinel
            nodes%boundary_size = nodes%boundary_add(1, nspmd + 1) - 1

            allocate(nodes%boundary(max(1, nodes%boundary_size)))

            do j = 1, nspmd
              pos = nodes%boundary_add(1, j)

              ! Recv sub-block: nodes rank J-1 owns that this rank ghosts:
              do k = recv_dsp(j) + 1, recv_dsp(j + 1)
                nodes%boundary(pos) = g2l(recv_buf(k))
                if(recv_buf(k) ==  13550) then
                  write(6,*) "spmd_rebuild_boundary: g2l(recv_buf) ", g2l(recv_buf(k)), " to boundary(", pos, ") for rank ", j-1
                  call flush(6)
                end if
                pos = pos + 1
              end do

              ! Send sub-block: nodes this rank owns that rank J-1 ghosts:
              do k = send_dsp(j) + 1, send_dsp(j + 1)
                nodes%boundary(pos) = g2l(send_buf(k))
                if(send_buf(k) ==  13550) then
                  write(6,*) "spmd_rebuild_boundary: g2l(send_buf) ", g2l(send_buf(k)), " to boundary(", pos, ") for rank ", j-1
                  call flush(6)
                end if
                pos = pos + 1
              end do
            end do

            ! ================================================================
            ! Cleanup
            ! ================================================================
            deallocate(ghost_gids, ghost_counts, ghost_displs, all_ghost)
            deallocate(g2l)
            deallocate(send_cnt, send_dsp, send_buf)
            deallocate(recv_cnt, recv_dsp, recv_buf)

          else
            ! Serial build: no ghost nodes, no boundary exchange needed
            if (allocated(nodes%boundary_add)) deallocate(nodes%boundary_add)
            if (allocated(nodes%boundary))     deallocate(nodes%boundary)

            allocate(nodes%boundary_add(2, nspmd + 1))
            nodes%boundary_add(1, :) = 1
            nodes%boundary_add(2, :) = 1
            nodes%boundary_size = 0
            allocate(nodes%boundary(1))
          endif

        end subroutine spmd_rebuild_boundary

      end module spmd_rebuild_boundary_mod
