Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
! usage example, see the test subroutine at the end of this file
#include "my_real.inc"
      module mpi_exch_load
        implicit none
#ifdef MPI
#include "mpif.h"
#ifdef MYREAL8
        integer, parameter :: REAL_TYPE = MPI_DOUBLE_PRECISION
#else
        integer, parameter :: REAL_TYPE = MPI_REAL
#endif
#endif
        integer, parameter :: MSG_TAG = 18000

! Each processor must have a type(load_local_buffer) array of size nspmd.
! Except for rank 0, the load exchange load_buf is empty.
        type load_local_buffer
          integer :: n                                   !< number of nodes to be exchanged
          integer, dimension(:), allocatable :: ids      !< local ids of the nodes to be exchanged
          integer, dimension(:), allocatable :: user_ids !< global user ids of the nodes to be exchanged
          my_real, dimension(:), allocatable :: values   !< values to be exchanged
        end type load_local_buffer
        type load_global_buffer
          integer :: n                                   !< number of nodes to be exchanged by processor 0
          integer, dimension(:), allocatable :: ids      !< global ids of the nodes to be exchanged by processor 0
          my_real, dimension(:), allocatable :: values   !< values to be exchanged by processor 0
        end type load_global_buffer
        type load_mpi_buffer
          integer :: ispmd                                    !< get a copy of ispmd to avoid passing it by argument to each subroutine call
          integer :: nspmd                                    !< get a copy of nspmd to avoid passing it by argument to each subroutine call
          integer, dimension(:), allocatable :: send_requests !< (size nspmd) mpi requests
          integer, dimension(:), allocatable :: recv_requests !< (size nspmd) mpi requests
          type(load_local_buffer) :: local                    !< allocated on every processor
          type(load_global_buffer), dimension(:), allocatable :: global !< (size nspmd) allocated on processor 0 only
        end type load_mpi_buffer
      contains

!! \brief allocate load_buf%ids, load_buf%user_ids and load_buf%values
        subroutine load_local_allocate(local, n)
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(load_local_buffer), intent(inout) :: local
        integer, intent(in) :: n
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
        allocate(local%ids(n))
        allocate(local%user_ids(n))
        allocate(local%values(3*n))
        local%n = n
        end subroutine load_local_allocate

!! \brief deallocate the load exchange local buffer
        subroutine load_local_deallocate(local)
        implicit none

! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(load_local_buffer), intent(inout) :: local
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
        if(allocated(local%ids)) then
          deallocate(local%ids)
        endif
        if(allocated(local%user_ids)) then
          deallocate(local%user_ids)
        endif
        if(allocated(local%values)) then
          deallocate(local%values)
        endif
        end subroutine load_local_deallocate

        subroutine load_global_allocate(send, n)
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(load_global_buffer), intent(inout) :: send
        integer, intent(in) :: n
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
        allocate(send%ids(n))
        allocate(send%values(3*n))
        send%n = n
        end subroutine load_global_allocate

        subroutine load_global_deallocate(send)
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(load_global_buffer), intent(inout) :: send
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
        if(allocated(send%ids)) then
          deallocate(send%ids)
        endif
        if(allocated(send%values)) then
          deallocate(send%values)
        endif
        end subroutine load_global_deallocate

!! \brief initialise the load exchange load_buf
!! \details, rank 0 has a list of user_ids, to bo boradcasted to all other ranks
        subroutine load_init_buffer(load_buf, itab, numnod, user_ids, n, ispmd, nspmd, weight)
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Includes
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(load_mpi_buffer), intent(inout) :: load_buf  !< load_buf to be initialised
        integer, intent(in) :: n !< number of user_ids on process 0
        integer, intent(in) :: ispmd !< mpi rank
        integer, intent(in) :: nspmd !< mpi size
        integer, intent(in) :: numnod !< number of node on the process
        integer, intent(in) :: user_ids(n) !< user ids to be sent to other ranks
        integer, intent(in) :: itab(numnod) !< user ids of the local nodes
        integer, optional, intent(in) :: weight(numnod) !< set to 0 for nodes not to be exchanged
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: n_global, sendbuf(nspmd)
        integer :: ierr
        integer, dimension(:), allocatable :: user_ids_global
        integer :: p,i
        integer :: hash_uid
        integer :: id
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
!!        rank 0 broadcast n to all other ranks
        load_buf%ispmd = ispmd
        load_buf%nspmd = nspmd
        allocate(load_buf%recv_requests(nspmd))
        allocate(load_buf%send_requests(nspmd))
        if(ispmd == 0) n_global = n
#ifdef MPI
        load_buf%recv_requests = MPI_REQUEST_NULL
        load_buf%send_requests = MPI_REQUEST_NULL
        sendbuf(1) = n_global
        call mpi_bcast(sendbuf(1), 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
        n_global = sendbuf(1)
#endif
        allocate(user_ids_global(n_global))
        if(ispmd == 0) user_ids_global(1:n_global) = user_ids(1:n_global)

#ifdef MPI
        call mpi_bcast(user_ids_global, n_global, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
#endif
        !fill load_buf%ids and load_buf%user_ids from user_ids_global
        if(present(weight)) then
          call load_fill_local_buffer(load_buf%local, itab, numnod, user_ids_global, n_global, weight=weight)
        else
          call load_fill_local_buffer(load_buf%local, itab, numnod, user_ids_global, n_global)
        endif

        !gather load_buf%(1:nspmd)%n on rank 0
#ifdef MPI
        ! number of nodes found on rank ispmd
        call mpi_gather(load_buf%local%n, 1, MPI_INTEGER, sendbuf(1), 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
        if (ispmd == 0) then
          allocate(load_buf%global(nspmd))
          do p = 1, nspmd
            call load_global_allocate(load_buf%global(p), sendbuf(p))
          end do
          ! Local copy for processor 0
          load_buf%global(1)%ids(1:load_buf%global(1)%n) = load_buf%local%user_ids(1:load_buf%local%n)
          ! mpi_irecv the load_buf%user_ids and load_buf%user_ids from all other ranks
          do p = 2, nspmd
            call mpi_irecv(load_buf%global(p)%ids, load_buf%global(p)%n, MPI_INTEGER, p-1,
     .                      MSG_TAG, MPI_COMM_WORLD, load_buf%recv_requests(p), ierr)
          end do
        else
          ! mpi_send load_buf%user_ids and load_buf%user_ids to rank 0
          call mpi_send(load_buf%local%user_ids, load_buf%local%n, MPI_INTEGER, 0, MSG_TAG, MPI_COMM_WORLD, ierr)
        endif
        if(ispmd == 0) then
          ! mpi_waitall for all mpi_irecv
          load_buf%recv_requests(ispmd+1) = MPI_REQUEST_NULL
          call mpi_waitall(nspmd, load_buf%recv_requests, MPI_STATUSES_IGNORE, ierr) !
          ! we must find ids from user_ids
          call c_new_hash(hash_uid, n)
          do i = 1, n
            call c_hash_insert(hash_uid, user_ids(i), i)
          enddo
          do p = 1, nspmd
            do i = 1, load_buf%global(p)%n
              id = load_buf%global(p)%ids(i)
              call c_hash_find(hash_uid, id, load_buf%global(p)%ids(i))
            enddo
          enddo
          call c_delete_hash(hash_uid)
        endif
#endif
        deallocate(user_ids_global)
        end subroutine load_init_buffer

!! \brief fill load_buf%ids and load_buf%user_ids from a list of user_ids
        subroutine load_fill_local_buffer(local, itab, numnod, user_ids, n, weight)
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(load_local_buffer), intent(inout) :: local !< recv to be filled
        integer, intent(in) :: numnod !< total number of nodes local to the process
        integer, intent(in) :: n !< number of user_ids to be searched for
        integer, intent(in) :: itab(numnod) !< user ids of the local nodes
        integer, intent(in) :: user_ids(n) !< user ids to be searched for
        integer, optional, intent(in) :: weight(numnod) !< set to 0 for nodes not to be exchanged
! ----------------------------------------------------------------------------------------------------------------------
!                                                  Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: i
        integer :: hash_uid_2_local
        integer :: local_id
        integer :: n_found
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
        !! create hash table to find local id from user id
        call c_new_hash(hash_uid_2_local, numnod)
        if(.not. present(weight)) then
          do i = 1, numnod
            call c_hash_insert(hash_uid_2_local, itab(i), i)
          end do
        else
          do i = 1, numnod
            if(weight(i) == 0) cycle
            call c_hash_insert(hash_uid_2_local, itab(i), weight(i))
          end do
        endif
        n_found = 0
        do i = 1, n
          local_id = -1
          call c_hash_find(hash_uid_2_local, user_ids(i), local_id)
          if(local_id > 0) then
            n_found = n_found + 1
          endif
        end do
        call load_local_allocate(local, n_found)
        n_found = 0
        do i = 1, n
          local_id = -1
          call c_hash_find(hash_uid_2_local, user_ids(i), local_id)
          if(local_id > 0) then
            n_found = n_found + 1
            local%ids(n_found) = local_id
            local%user_ids(n_found) = user_ids(i)
          endif
        enddo
        call c_delete_hash(hash_uid_2_local)
        end subroutine load_fill_local_buffer

        subroutine load_scatter_values(load_buf, values_global, n)
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(load_mpi_buffer), intent(inout) :: load_buf !< mpi buffer
        integer, intent(in) :: n !< number of values to be scattered
        my_real, intent(in) :: values_global(3*n) !< values to be scattered, in the same order as user_ids
! ----------------------------------------------------------------------------------------------------------------------
!                                                  Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: i,j,p
        integer :: ierr
        integer :: ispmd, nspmd
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

        ispmd = load_buf%ispmd
        nspmd = load_buf%nspmd

        if(ispmd == 0) then

          ! copy for p == ispmd == 0
          do i = 1,load_buf%global(1)%n
            j = 3* (load_buf%global(1)%ids(i)-1)
            load_buf%local%values(3*(i-1)+1) = values_global(j+1)
            load_buf%local%values(3*(i-1)+2) = values_global(j+2)
            load_buf%local%values(3*(i-1)+3) = values_global(j+3)
          enddo
          do p = 2, nspmd
            ! copy the values to be sent to processor i
            do i = 1, load_buf%global(p)%n
              j = 3* (load_buf%global(p)%ids(i)-1)
              load_buf%global(p)%values(3*(i-1)+1) = values_global(j+1)
              load_buf%global(p)%values(3*(i-1)+2) = values_global(j+2)
              load_buf%global(p)%values(3*(i-1)+3) = values_global(j+3)
            enddo
            ! isend the values to processor i
            j = 3 * load_buf%global(p)%n
#ifdef MPI
            call mpi_isend(load_buf%global(p)%values, j, REAL_TYPE, p-1,
     .                    MSG_TAG, MPI_COMM_WORLD, load_buf%send_requests(p), ierr)
#endif
          enddo
        else
          ! all other processors : mpi_recv
          j = 3 * load_buf%local%n
#ifdef MPI
          call mpi_recv(load_buf%local%values,j, REAL_TYPE, 0,
     .                 MSG_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr) ! 1
#endif
        endif

#ifdef MPI
        if(ispmd == 0) then
          call mpi_waitall(nspmd, load_buf%recv_requests, MPI_STATUSES_IGNORE, ierr)
        endif
#endif

        end subroutine load_scatter_values

        subroutine load_gather_values(load_buf, values_global, n)
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(load_mpi_buffer), intent(inout) :: load_buf !< mpi buffer
        integer, intent(in) :: n !< number of values to be gathered
        my_real, intent(inout) :: values_global(n) ! the global values
! ----------------------------------------------------------------------------------------------------------------------
!                                                  Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: i,j,p
        integer :: ierr
        integer :: ispmd, nspmd
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

        ispmd = load_buf%ispmd
        nspmd = load_buf%nspmd
#ifdef MPI
        if( ispmd == 0) then
          do p = 2, nspmd
            ! copy the values to be sent to processor i
            ! mpi_irecv all the values
            j = 3 * load_buf%global(p)%n
            call mpi_irecv(load_buf%global(p)%values, j, REAL_TYPE, p-1,
     .                      MSG_TAG, MPI_COMM_WORLD, load_buf%recv_requests(p), ierr)
          enddo
        else
          ! call mpi_isend the values to processor 0
          j = 3 * load_buf%local%n
          call mpi_isend(load_buf%local%values, j, REAL_TYPE, 0,
     .                   MSG_TAG, MPI_COMM_WORLD, load_buf%send_requests(1), ierr)
        endif

        call mpi_wait(load_buf%send_requests(1), MPI_STATUS_IGNORE, ierr)
        call mpi_waitall(nspmd, load_buf%recv_requests, MPI_STATUSES_IGNORE, ierr)
#endif
        if(ispmd == 0) then
          ! case p == 1 , transfer directly local%values to values_global
          do i = 1, load_buf%global(1)%n
            j = 3 * (load_buf%global(1)%ids(i)-1)
            values_global(j+1) = load_buf%local%values(3*(i-1)+1)
            values_global(j+2) = load_buf%local%values(3*(i-1)+2)
            values_global(j+3) = load_buf%local%values(3*(i-1)+3)
          enddo

          ! fill values_global from load_buf%global(p)%values
          do p = 2,nspmd
            do i = 1, load_buf%global(p)%n
              j = 3 * (load_buf%global(p)%ids(i)-1)
              values_global(j+1) = load_buf%global(p)%values(3*(i-1)+1)
              values_global(j+2) = load_buf%global(p)%values(3*(i-1)+2)
              values_global(j+3) = load_buf%global(p)%values(3*(i-1)+3)
            enddo
          enddo
        endif
        end subroutine load_gather_values
      end module mpi_exch_load




! test subroutine
      subroutine test_comm_load()
      use mpi_exch_load
      implicit none
#ifdef MPI
      integer :: ispmd ! MPI_RANK
      integer :: nspmd ! MPI_SIZE
      integer :: ierr
      integer :: i,n
      integer, dimension(:), allocatable :: user_ids
      type(load_mpi_buffer) :: load_buf
      integer, dimension(:), allocatable :: itab
      integer :: numnod
      my_real, dimension(:), allocatable :: values_global, values_global2
      my_real, dimension(:), allocatable :: values_local

      !find the rank
      call mpi_comm_rank(MPI_COMM_WORLD, ispmd, ierr)
      call mpi_comm_size(MPI_COMM_WORLD, nspmd, ierr)

      if(ispmd == 0) then
        n = 10
        allocate(user_ids(n))
        allocate(values_global(3*n)) ! array containing the forces on processor 0
        user_ids = (/1,2,3,4,50,6,7,8,9,101/) ! user ids of the forces gathered on processor 0
        do i = 1,n
          values_global(3*(i-1)+1) = 1.0 * user_ids(i)
          values_global(3*(i-1)+2) = 10.0 * user_ids(i)
          values_global(3*(i-1)+3) = 100.0 * user_ids(i)
        enddo
        ! allocate values_global2 to be a copy of values_global
        allocate(values_global2(3*n))
        values_global2 = values_global
      else
        n = 0
        allocate(user_ids(n))
        allocate(values_global(3*n))
      endif

      numnod = 5
      allocate(itab(numnod))

      if(ispmd == 0) then
        itab = (/1,2,30,40,50/)
      else if(ispmd == 1) then
        itab = (/101,102,3,4,5/)
      else if(ispmd == 2) then
        itab = (/6,7,8,9,10/)
      else
        numnod = 0
      endif
      allocate(values_local(3*numnod))
      do i = 1,numnod
        values_local(3*(i-1)+1) = 1.0 * itab(i)
        values_local(3*(i-1)+2) = 10.0 * itab(i)
        values_local(3*(i-1)+3) = 100.0 * itab(i)
      enddo

      ! INITIALIZATION :
      ! the call to load_init_buffer has to be done on both runs (read and write forces values)
      ! it allocates and fills load_buf which contains the communication pattern
      ! input:
      ! itab: (on all processors) the list of the user ids on the current processor
      ! numnod: (on all processors) the number of nodes on the current processor
      ! user_ids: (on processor 0) the list of the global ids that will be written and read from the file
      ! n : (on processor 0) the number of global ids that will be written and read from the file
      ! output:
      ! load_buf: (on all processors) the communication pattern
      !    load_buf%local%ids: the local ids of the nodes to be exchanged with processor 0
      !    load_buf%local%user_ids: the global ids of the nodes to be exchanged with processor 0
      !    load_buf%global(p)%ids: (only on processor 0) the user ids of the nodes to be exchanged with processor p
      call load_init_buffer(load_buf, itab, numnod, user_ids, n, ispmd, nspmd)

      ! RUN 2 : processor 0 has read values_global from a file and sends the values to all other processors
      ! input : (on processor 0) values_global(n)
      ! output: (on all processors) load_buf%local%values(1:3*load_buf%local%n)
      call load_scatter_values(load_buf, values_global, n)

      ! RUN 1: processor 0 gathers the values from all other processors, before writting them to a file
      ! all the processors fills load_buf%recvs%values and it is gathered on processor 0
      ! values_local is an array of "forces" of size 3*numnod, we copy the values to load_buf%local%values
      ! before calloing load_gather_values
      do i = 1, load_buf%local%n
        !                                  values_local(1:numnod)
        load_buf%local%values(3*(i-1)+1) = values_local(3*(load_buf%local%ids(i)-1)+1)
        load_buf%local%values(3*(i-1)+2) = values_local(3*(load_buf%local%ids(i)-1)+2)
        load_buf%local%values(3*(i-1)+3) = values_local(3*(load_buf%local%ids(i)-1)+3)
      enddo
      values_global = 0.0 ! flush the previous values
      ! input : (on all processors) load_buf%local%values
      ! output : (on processor 0) values_global(n)
      call load_gather_values(load_buf, values_global, n)

      !check that values_global == values_global2 and print a message

      if(ispmd == 0) then
        if(all(values_global == values_global2)) then
          write(6,*) 'ispmd = ', ispmd, ' test OK'
          ! print values and values global
        else
          write(6,*) 'ispmd = ', ispmd, ' test FAILED'
          do i = 1,n
            write(6,*) 'values_global(',i,') = ', values_global(3*(i-1)+1), values_global(3*(i-1)+2), values_global(3*(i-1)+3)
            write(6,*) 'values_global2(',i,') = ', values_global2(3*(i-1)+1), values_global2(3*(i-1)+2), values_global2(3*(i-1)+3)
          enddo

        endif
      endif

#endif
      end subroutine
