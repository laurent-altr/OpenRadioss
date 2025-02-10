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
      module inter_shm_mod
        use iso_c_binding
#ifdef MPI
#include  "mpif.h"
#else
#define MPI_ADDRESS_KIND 8
#define MPI_UNDEFINED -1
#endif
        private

        public :: inter_win_
        public :: inter_window_open
        public :: inter_window_update
        public :: inter_window_close
        public :: nullify_inter_win

        type inter_win_
          integer :: rank_inter, size_inter
          integer :: rank_intra_node, size_node
          integer :: rank_inter_node, number_of_nodes
          integer :: COMM_INTER_NODE
          integer :: COMM_INTRA_NODE
          integer :: node_id
          integer :: disp_unit !                                           
          integer :: win
          TYPE(C_PTR) :: shared_base
          integer, dimension(:,:), allocatable :: ranks_mapping
          integer, dimension(:), allocatable :: ranks_mapping_reverse
          double precision, dimension(:), pointer :: shared_data
          integer, dimension(:), allocatable :: starting_node_of_proc
          integer, dimension(:), allocatable :: nsns_in_world
          integer(MPI_ADDRESS_KIND) :: shared_size ! in bytes
          integer(MPI_ADDRESS_KIND), dimension(:,:), allocatable :: addresses
          ! data necessary to ALLGATHERV of the global shared_data in COMM_INTER_NODE
          integer sendcount
          integer, dimension(:), allocatable :: recvcounts
          integer, dimension(:), allocatable :: displs
          integer :: nsn_global
          integer :: rsiz, isiz
          integer :: ielec_offset, temp_offset, areas_offset, gap_s_offset, gap_sl_offset, ipartfrics_offset
          integer :: x_offset, v_offset, ms_offset, stfns_offset
          integer :: i_offset, ispmd_offset, itab_offset, IKINET_offset
        end type inter_win_

!       interface
!               function sched_getcpu() bind(C, name="sched_getcpu")
!                 use, intrinsic :: iso_c_binding
!                   integer(c_int) :: sched_getcpu
!               end function sched_getcpu
!        function numa_node_of_cpu(cpu) bind(C, name="numa_node_of_cpu")
!               use, intrinsic :: iso_c_binding
!               integer(c_int), value :: cpu
!               integer(c_int) :: numa_node_of_cpu
!           end function numa_node_of_cpu
!     end interface

      contains


      subroutine nullify_inter_win(inter_win_var)
          use, intrinsic :: iso_c_binding, only: c_null_ptr
          implicit none
#include "mpif.h"
          type(inter_win_) :: inter_win_var
        
          ! -- Set all standard integer fields to 0
          inter_win_var%rank_inter       = MPI_PROC_NULL
          inter_win_var%size_inter       = 0
          inter_win_var%rank_intra_node  = MPI_PROC_NULL
          inter_win_var%size_node        = 0
          inter_win_var%rank_inter_node  = MPI_PROC_NULL
          inter_win_var%number_of_nodes  = 0
          inter_win_var%node_id          = 0
          inter_win_var%disp_unit        = 0
          inter_win_var%sendcount        = 0
          inter_win_var%nsn_global       = 0
          inter_win_var%rsiz             = 0
          inter_win_var%isiz             = 0
          inter_win_var%ielec_offset     = 0
          inter_win_var%temp_offset      = 0
          inter_win_var%areas_offset     = 0
          inter_win_var%gap_s_offset     = 0
          inter_win_var%gap_sl_offset    = 0
          inter_win_var%ipartfrics_offset= 0
          inter_win_var%x_offset         = 0
          inter_win_var%v_offset         = 0
          inter_win_var%ms_offset        = 0
          inter_win_var%stfns_offset     = 0
          inter_win_var%i_offset         = 0
          inter_win_var%ispmd_offset     = 0
          inter_win_var%itab_offset      = 0
          inter_win_var%IKINET_offset    = 0
        
          ! -- Set MPI_ADDRESS_KIND fields to 0
          inter_win_var%shared_size      = 0_MPI_ADDRESS_KIND
        
          ! -- Set MPI communicators to MPI_COMM_NULL
          inter_win_var%COMM_INTER_NODE  = MPI_COMM_NULL
          inter_win_var%COMM_INTRA_NODE  = MPI_COMM_NULL
        
          ! -- Set the MPI window to MPI_WIN_NULL
          inter_win_var%win              = MPI_WIN_NULL
        
          ! -- Nullify the C pointer
          inter_win_var%shared_base      = c_null_ptr
        
          ! -- Nullify the Fortran pointer
          nullify(inter_win_var%shared_data)
        
          ! -- Allocatable arrays need no explicit initialization;
          !    they start out not allocated.
        
          return
      end subroutine nullify_inter_win 

        ! initialize the inter-node window
         subroutine set_win_size(inter_win,  intbuf_tab, ipari,npari)
          use intbufdef_mod
          use iso_c_binding
          implicit none
#ifdef MPI
#include  "mpif.h"
#else
#define MPI_ADDRESS_KIND 8
#endif

! input arguments
          integer, intent(in) :: npari
          type(inter_win_), intent(inout) :: inter_win
          type(intbuf_struct_), intent(in) :: intbuf_tab
          integer, dimension(npari), intent(in) :: ipari
! local variables
          integer :: igap, intth, intfric, ityp, itied, nmn, nsn, inacti, ifq
          integer :: lr, li           
          integer :: ielec_offset, temp_offset, areas_offset, gap_s_offset, gap_sl_offset, ipartfrics_offset
          integer :: ispmd_offset, itab_offset, IKINET_offset, i_offset 
          igap = ipari(21)
          intth = ipari(47)
          intfric = ipari(72)
          ityp = ipari(7)
          itied = ipari(85)
          nmn = ipari(6)
          nsn = ipari(5)
          inacti = ipari(22)
          ifq =ipari(31)

          ielec_offset = 0
          temp_offset = 0
          areas_offset = 0 
          gap_s_offset = 0 
          gap_sl_offset =0
          ipartfrics_offset = 0

          lr = 8
          if(igap==1 .or. igap==2)then
            gap_s_offset = lr + 1
            lr = lr + 1
          elseif(igap==3)then 
            gap_s_offset = lr + 1
            gap_sl_offset = lr + 2
            lr = lr + 2
          endif
          if(intth>0)then
             temp_offset = lr + 1
             areas_offset = lr + 2
             lr = lr + 2
          endif
          inter_win%rsiz = lr 

          ispmd_offset = lr + 1
          itab_offset = lr + 2
          IKINET_offset = lr + 3
          i_offset = lr + 4
          ! =====
          li = lr +4
          if(intth>0)then
             ielec_offset = li + 1
             li = li + 1
          endif
          if(intfric>0)then
             ipartfrics_offset = li + 1
             li = li + 1
          endiF              

          inter_win%isiz = li - lr
          inter_win%ielec_offset = ielec_offset
          inter_win%temp_offset = temp_offset
          inter_win%areas_offset = areas_offset 
          inter_win%gap_s_offset = gap_s_offset 
          inter_win%gap_sl_offset = gap_sl_offset
          inter_win%ipartfrics_offset = ipartfrics_offset
          inter_win%x_offset = 1
          inter_win%v_offset = 4
          inter_win%ms_offset = 7
          inter_win%stfns_offset = 8
          inter_win%ispmd_offset = ispmd_offset
          inter_win%itab_offset = itab_offset
          inter_win%ikinet_offset = IKINET_offset
          inter_win%i_offset = i_offset
        end subroutine set_win_size 


        subroutine inter_window_open(inter_win, n_local, intbuf_tab, rank_world, nspmd, ipari, npari)
          use iso_c_binding
          use intbufdef_mod
          implicit none
#ifdef MPI
#include  "mpif.h"
#endif
! input arguments
          integer :: nspmd ! number of spmd processes
          integer, intent(in) :: npari
          integer, dimension(npari), intent(in) :: ipari
          type(inter_win_), intent(inout) :: inter_win
          integer, intent(in) :: rank_world
          integer, intent(in) :: n_local
          type(intbuf_struct_), intent(in) :: intbuf_tab
! local variables
          integer :: mpi_err
          integer :: rank, rankmin
          integer :: color
          integer, parameter :: disp_unit  = 8
          integer(MPI_ADDRESS_KIND) :: i,j,k,ibegin
          integer(MPI_ADDRESS_KIND) :: mpi_zero
          integer :: n_global
          integer :: integer_count, real_count
          integer :: inter_comm

!         integer :: cpu_id
#ifdef MPI
          inter_comm = intbuf_tab%MPI_COMM
          call set_win_size(inter_win, intbuf_tab, ipari,npari)
          integer_count = inter_win%isiz
          real_count =inter_win%rsiz

          call mpi_comm_rank(inter_comm, inter_win%rank_inter, mpi_err)
          call mpi_comm_size(inter_comm, inter_win%size_inter, mpi_err)

!         cpu_id = numa_node_of_cpu(sched_getcpu())
!         call MPI_Comm_split(inter_comm, cpu_id, &
!           inter_win%rank_inter, &
!           inter_win%COMM_INTRA_NODE, mpi_err)

          call MPI_Comm_split_type(inter_comm, MPI_COMM_TYPE_SHARED, &
            inter_win%rank_inter, MPI_INFO_NULL, &
            inter_win%COMM_INTRA_NODE, mpi_err)

          call MPI_Comm_rank(inter_win%COMM_INTRA_NODE, inter_win%rank_intra_node, mpi_err)
          call mpi_comm_size(inter_win%COMM_INTRA_NODE, inter_win%size_node, mpi_err)


          ! Create inter-node communicator (inter_node_comm)
          if (inter_win%rank_intra_node == 0) then
            ! Assign a unique color for rank_node == 0 (leader rank)
            color = 1
          else
            ! Exclude non-leader ranks from inter_node_comm
            color = MPI_UNDEFINED
          end if

          ! Split the world communicator using the color
          call MPI_Comm_split(inter_comm, color, inter_win%rank_inter, inter_win%COMM_INTER_NODE, mpi_err)

          ! Determine the rank in inter_node_comm (only for participating ranks)
          if (color /= MPI_UNDEFINED) then
            call mpi_comm_size(inter_win%COMM_INTER_NODE, inter_win%number_of_nodes, mpi_err)
            call MPI_Comm_rank(inter_win%COMM_INTER_NODE, inter_win%rank_inter_node, mpi_err)
          else
            inter_win%number_of_nodes = 0
            inter_win%rank_inter_node = MPI_UNDEFINED
          end if

!          write(6,*) "RANKS",inter_win%rank_inter, inter_win%rank_intra_node, inter_win%rank_inter_node

          !all_reduce summ of n_local => shared_size

          ! node_id =  rank of (rank == 0 in the intra-node communicator) in the inter-node communicator
          ! broadcast node_id to all ranks in the intra-node communicator
          inter_win%node_id = inter_win%rank_inter_node
          call MPI_Bcast(inter_win%node_id, 1, MPI_INTEGER, 0, inter_win%COMM_INTRA_NODE, mpi_err)

          allocate(inter_win%ranks_mapping(4, inter_win%size_inter))
          
          inter_win%ranks_mapping(1:4,:) = 0
          inter_win%ranks_mapping(1, inter_win%rank_inter+1) = inter_win%rank_intra_node
          inter_win%ranks_mapping(2, inter_win%rank_inter+1) = inter_win%node_id
          inter_win%ranks_mapping(3, inter_win%rank_inter+1) = n_local
          inter_win%ranks_mapping(4, inter_win%rank_inter+1) = rank_world

          ! allreduce ranks_mapping
          call MPI_ALLREDUCE(MPI_IN_PLACE, inter_win%ranks_mapping, &
            inter_win%size_inter*4, MPI_INTEGER, MPI_SUM, &
            inter_comm, mpi_err)

          ! Allocate the rank mapping arrays
          allocate(inter_win%nsns_in_world(nspmd))
          inter_win%nsns_in_world(1:nspmd) = 0
          do i = 1,inter_win%size_inter
            inter_win%nsns_in_world(inter_win%ranks_mapping(4,i)+1) = inter_win%ranks_mapping(3,i)
          end do

          call MPI_ALLREDUCE(n_local, n_global, 1, MPI_INTEGER, MPI_SUM, inter_comm, mpi_err)
!         write(6,*) "shared_size",n_global,disp_unit, integer_count, real_count
          inter_win%shared_size = n_global* disp_unit * (integer_count + real_count)
          inter_win%nsn_global= n_global

          if (inter_win%rank_intra_node == 0) then
            call MPI_Win_allocate_shared(inter_win%shared_size, disp_unit, MPI_INFO_NULL, &
              inter_win%COMM_INTRA_NODE, inter_win%shared_base, inter_win%win, mpi_err)
          else
            mpi_zero = 0_MPI_ADDRESS_KIND
            call MPI_Win_allocate_shared(mpi_zero, disp_unit, MPI_INFO_NULL, &
              inter_win%COMM_INTRA_NODE, inter_win%shared_base, &
              inter_win%win, mpi_err)
            ! Query the shared memory to get the pointer
            call MPI_Win_shared_query(inter_win%win, 0, inter_win%shared_size, inter_win%disp_unit, inter_win%shared_base, mpi_err)

          end if

          ! Convert base pointer to a usable Fortran pointer
          call c_f_pointer(inter_win%shared_base, inter_win%shared_data, [inter_win%shared_size / disp_unit])


          allocate(inter_win%addresses(inter_win%size_inter,2))
          allocate(inter_win%ranks_mapping_reverse(inter_win%size_inter))
          inter_win%addresses = 0_MPI_ADDRESS_KIND

          ! The ranks may not be packed per node: ie ranks 0 to n  on the first node and so on.
          ! We define a mapping between the ranks and the addresses in the shared memory, which should be contiguous
          !   |               node 0                |          node 1         |                node 2                | ! node id
          !   |   core 0  |   core 1   |   core 2   |   core 0   |   core 1   |   core 0   |   core 1   |   core 2   | ! rank_intra_node
          !   !        0           3            4   |        1            4   |        2            5            9   | ! rank_inter (no assumption)
          !  The addressses in the shared memory should be contiguous per node, so we need a mapping between the ranks and the addresses
          ibegin = 1_MPI_ADDRESS_KIND
          rank = 0
          rankmin = HUGE(rankmin)
          k = 0
          allocate(inter_win%starting_node_of_proc(nspmd))
          inter_win%starting_node_of_proc = -1
          do j = 1,inter_win%size_inter
            do i = 1,inter_win%size_inter
              ! find the smallest "rank" not allready in the addresses
              ! Here "Rank" corresponds to the numbering when the processes of the same node are clustered together
              ! inter_win%size_inter * node_id + rank_intra_node
              rank = inter_win%size_inter *(1+inter_win%ranks_mapping(2,i)) + inter_win%ranks_mapping(1,i)
              if(rank < rankmin .and. inter_win%addresses(i,1) == 0_MPI_ADDRESS_KIND) then
                rankmin = rank
                k = i ! old2new(j) = k
              end if
            end do
            ! rankmin is the smallest rank not allready in the addresses
            inter_win%addresses(j,1) = ibegin
            inter_win%starting_node_of_proc(inter_win%ranks_mapping(4,k)+1) = (ibegin-1) / (integer_count + real_count) 
            ibegin = ibegin + inter_win%ranks_mapping(3,k) * (integer_count + real_count)! nlocal
            inter_win%ranks_mapping_reverse(k) = j-1 !
            inter_win%addresses(j,2) = ibegin - 1
            rankmin=HUGE(rankmin)
            k = 0
          enddo
!         do j = 1,inter_win%size_inter
!           inter_win%addresses(j,1) = ibegin
!           ibegin = ibegin + inter_win%ranks_mapping(3,j) ! nlocal
!           inter_win%addresses(j,2) = ibegin - 1
!         enddo

          if(inter_win%rank_intra_node == 0) then
            ! Allocate memory for the ALLGATHERV
            allocate(inter_win%recvcounts(inter_win%number_of_nodes))
            allocate(inter_win%displs(inter_win%number_of_nodes))
            inter_win%recvcounts = 0
            inter_win%displs = 0
            do i = 1,inter_win%size_inter
              k = inter_win%ranks_mapping(2,i)+1   !node id
              inter_win%recvcounts(k) = inter_win%recvcounts(k) + inter_win%ranks_mapping(3,i)*(integer_count + real_count)
            end do
            inter_win%displs(1) = 0
            do i = 2,inter_win%number_of_nodes
              inter_win%displs(i) = inter_win%displs(i-1) + inter_win%recvcounts(i-1)
            end do
          end if
          write(6,*) rank_world,"PROCESS", inter_win%rank_inter, inter_win%rank_intra_node, inter_win%rank_inter_node
          write(6,*) rank_world,"addresses",inter_win%addresses(:,1),inter_win%addresses(:,2)
          write(6,*) "MPI communicators:", inter_comm, inter_win%COMM_INTER_NODE, inter_win%COMM_INTRA_NODE
           call flush(6)
          call MPI_Barrier(inter_comm, mpi_err)
#endif
        end subroutine

        
        subroutine inter_window_update(inter_win, n_local, local_data)
          use iso_c_binding
          implicit none
#ifdef MPI
#include  "mpif.h"
#endif

! input arguments
          type(inter_win_), intent(inout) :: inter_win
          integer, intent(in) :: n_local
          double precision, dimension(:), intent(in) :: local_data
! local variables
          integer :: mpi_err
          integer(MPI_ADDRESS_KIND) :: i, ibegin,iend
          double precision, dimension(:), pointer, volatile :: shared_data
#ifdef MPI

          shared_data => inter_win%shared_data
          call MPI_Barrier(inter_win%COMM_INTRA_NODE, mpi_err)

          ! Barrier
          ! Update shared memory
          ibegin = inter_win%addresses(inter_win%rank_inter+1,1)
          iend = inter_win%addresses(inter_win%rank_inter+1,2)
          do i = ibegin, iend
            shared_data(i) = local_data(i - ibegin + 1)
          end do
          call MPI_Barrier(inter_win%COMM_INTRA_NODE, mpi_err)

          if(inter_win%rank_intra_node == 0) then
            call MPI_Allgatherv(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, &
              shared_data, inter_win%recvcounts, inter_win%displs, MPI_DOUBLE_PRECISION, &
              inter_win%COMM_INTER_NODE, mpi_err)
          end if

#endif
        end subroutine inter_window_update

        subroutine inter_window_close(inter_win)
          use iso_c_binding
          implicit none
#ifdef MPI
#include  "mpif.h"
#endif
! input arguments
          type(inter_win_), intent(inout) :: inter_win
! local variables
          integer :: mpi_err

#ifdef MPI
          call MPI_Win_free(inter_win%win, mpi_err)
          deallocate(inter_win%ranks_mapping)
          deallocate(inter_win%addresses)
          if(allocated(inter_win%recvcounts)) deallocate(inter_win%recvcounts)
          if(allocated(inter_win%displs)) deallocate(inter_win%displs)
          if(allocated(inter_win%ranks_mapping_reverse)) deallocate(inter_win%ranks_mapping_reverse)

          if(inter_win%COMM_INTER_NODE /= MPI_COMM_NULL) then
            call MPI_Comm_free(inter_win%COMM_INTER_NODE, mpi_err)
          end if

          if(inter_win%COMM_INTRA_NODE /= MPI_COMM_NULL) then
            call MPI_Comm_free(inter_win%COMM_INTRA_NODE, mpi_err)
          end if
#endif
        end subroutine inter_window_close
      end module
