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
!||    spmd_rebuild_boundary_mod   ../engine/source/mpi/nodes/spmd_rebuild_boundary.F90
!||--- called by ------------------------------------------------------
!||    resol                       ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    spmd_allgatherv_mod         ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_comm_world_mod         ../engine/source/mpi/spmd_comm_world.F90
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
!! A, V, X, ...).  BOUNDARY_ADD(1, J) is the 1-based CSR start index
!! for rank J-1 (0-based MPI rank), so rank J-1's nodes occupy
!!   BOUNDARY( BOUNDARY_ADD(1,J) : BOUNDARY_ADD(1,J+1)-1 ).
!!
!! \param[in]  numnod        local number of nodes on this MPI rank
!! \param[in]  nspmd         total number of MPI ranks
!! \param[in]  ispmd         this rank's 0-based MPI rank
!! \param[in]  w(numnod)     ownership: 1 = owned by this rank, 0 = ghost
!! \param[in]  itab(numnod)  global unique node ID for each local node
!! \param[out] boundary      flat list of local node indices grouped by rank;
!!                           (re)allocated here, caller is responsible for
!!                           deallocation if previously allocated
!! \param[out] boundary_add  CSR index array, shape (1, nspmd+1)
!! \param[out] boundary_size total number of entries in boundary

subroutine spmd_rebuild_boundary(numnod, nspmd, ispmd, w, itab, &
                                 boundary, boundary_add, boundary_size)

  use spmd_allgatherv_mod,  only : spmd_allgatherv
  use spmd_comm_world_mod,  only : spmd_comm_world

  implicit none
#include "spmd.inc"

  ! ------------------------------------------------------------------ arguments
  integer, intent(in)  :: numnod
  integer, intent(in)  :: nspmd
  integer, intent(in)  :: ispmd                  ! 0-based MPI rank
  integer, intent(in)  :: w(numnod)
  integer, intent(in)  :: itab(numnod)
  integer, allocatable, intent(out) :: boundary(:)
  integer, allocatable, intent(out) :: boundary_add(:,:)
  integer, intent(out) :: boundary_size

  ! ------------------------------------------------------------------ locals
  integer :: i, j, k, gid, lidx, ierr
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

#ifdef MPI

  ! ================================================================
  ! Phase 1 — collect ghost global IDs on this rank
  ! ================================================================
  n_ghost_local = 0
  do i = 1, numnod
    if (w(i) == 0) n_ghost_local = n_ghost_local + 1
  end do

  allocate(ghost_gids(max(1, n_ghost_local)))
  k = 0
  do i = 1, numnod
    if (w(i) == 0) then
      k = k + 1
      ghost_gids(k) = itab(i)
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

  call MPI_Allgather(n_ghost_local, 1, MPI_INTEGER, &
                     ghost_counts,  1, MPI_INTEGER, &
                     spmd_comm_world, ierr)

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
    if (itab(i) > max_gid_local) max_gid_local = itab(i)
  end do
  call MPI_Allreduce(max_gid_local, max_gid, 1, MPI_INTEGER, MPI_MAX, &
                     spmd_comm_world, ierr)

  allocate(g2l(max_gid))
  g2l(:) = 0
  do i = 1, numnod
    g2l(itab(i)) = i
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
      if (lidx > 0 .and. w(lidx) == 1) &               ! I own this node
        send_cnt(j) = send_cnt(j) + 1
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
      if (lidx > 0 .and. w(lidx) == 1) then
        pos = pos + 1
        send_buf(pos) = gid
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
  call MPI_Alltoall(send_cnt, 1, MPI_INTEGER, &
                    recv_cnt, 1, MPI_INTEGER, &
                    spmd_comm_world, ierr)

  recv_dsp(1) = 0
  do j = 1, nspmd
    recv_dsp(j + 1) = recv_dsp(j) + recv_cnt(j)
  end do
  r_tot = recv_dsp(nspmd + 1)

  allocate(recv_buf(max(1, r_tot)))

  ! send_dsp(1:nspmd) and recv_dsp(1:nspmd) are the 0-based MPI
  ! displacements for ranks 0..nspmd-1 respectively.
  call MPI_Alltoallv(send_buf, send_cnt, send_dsp(1:nspmd), MPI_INTEGER, &
                     recv_buf, recv_cnt, recv_dsp(1:nspmd), MPI_INTEGER, &
                     spmd_comm_world, ierr)

  ! ================================================================
  ! Phase 6 — assemble BOUNDARY and BOUNDARY_ADD
  !
  ! For rank J-1 (1-indexed J):
  !   send_buf segment → nodes this rank owns that rank J-1 ghosts
  !   recv_buf segment → nodes rank J-1 owns that this rank ghosts
  ! Both use LOCAL node indices in BOUNDARY (via g2l).
  ! ================================================================
  if (allocated(boundary_add)) deallocate(boundary_add)
  if (allocated(boundary))     deallocate(boundary)

  allocate(boundary_add(1, nspmd + 1))
  boundary_add(1, 1) = 1
  do j = 1, nspmd
    boundary_add(1, j + 1) = boundary_add(1, j) + send_cnt(j) + recv_cnt(j)
  end do
  boundary_size = boundary_add(1, nspmd + 1) - 1

  allocate(boundary(max(1, boundary_size)))

  do j = 1, nspmd
    pos = boundary_add(1, j)

    ! Nodes this rank owns that rank J-1 ghosts:
    do k = send_dsp(j) + 1, send_dsp(j + 1)
      boundary(pos) = g2l(send_buf(k))
      pos = pos + 1
    end do

    ! Nodes rank J-1 owns that this rank ghosts:
    do k = recv_dsp(j) + 1, recv_dsp(j + 1)
      boundary(pos) = g2l(recv_buf(k))
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

#else
  ! Serial build: no ghost nodes, no boundary exchange needed
  if (allocated(boundary_add)) deallocate(boundary_add)
  if (allocated(boundary))     deallocate(boundary)

  allocate(boundary_add(1, nspmd + 1))
  boundary_add(1, :) = 1
  boundary_size = 0
  allocate(boundary(1))
#endif

end subroutine spmd_rebuild_boundary

end module spmd_rebuild_boundary_mod
