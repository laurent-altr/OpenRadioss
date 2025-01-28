module inter_node_window
    use mpi
    use iso_c_binding
    type inter_win_
      integer :: rank_world, size_world
      integer :: rank_intra_node
      integer :: rank_inter_node
      integer :: COMM_INTER_NODE
      integer :: COMM_INTRA_NODE
      integer :: node_id
      integer :: disp_unit
      integer :: win
      TYPE(C_PTR) :: shared_base
      integer, dimension(:,:), allocatable :: ranks_mapping
      double precision, dimension(:), pointer :: shared_data
      integer(MPI_ADDRESS_KIND) :: shared_size
      integer(MPI_ADDRESS_KIND), dimension(:), allocatable :: addresses
    end type inter_win_
    contains
    ! initialize the inter-node window 
    subroutine inter_window_open(inter_win, n_local, spmd_comm_world)
        use mpi
        use iso_c_binding
        implicit none
! input arguments
        type(inter_win_), intent(inout) :: inter_win
        integer, intent(in) :: spmd_comm_world
        integer, intent(in) :: n_local
! local variables
        integer :: mpi_err
        integer :: rank
        integer :: color
        integer, parameter :: disp_unit = 8
        integer(MPI_ADDRESS_KIND) :: i,ibegin
        integer(MPI_ADDRESS_KIND) :: mpi_zero

        call mpi_comm_rank(spmd_comm_world, inter_win%rank_world, mpi_err)
        call mpi_comm_size(spmd_comm_world, inter_win%size_world, mpi_err)

        call MPI_Comm_split_type(SPMD_COMM_WORLD, MPI_COMM_TYPE_SHARED, &
                     inter_win%rank_world, MPI_INFO_NULL, &
                     inter_win%COMM_INTRA_NODE, mpi_err)
        call MPI_Comm_rank(inter_win%COMM_INTRA_NODE, inter_win%rank_intra_node, mpi_err)

        ! Step 2: Create inter-node communicator (inter_node_comm)
        if (inter_win%rank_intra_node == 0) then
            ! Assign a unique color for rank_node == 0 (leader rank)
            color = inter_win%rank_world
        else
            ! Exclude non-leader ranks from inter_node_comm
            color = MPI_UNDEFINED
        end if
    
        ! Split the world communicator using the color
        call MPI_Comm_split(SPMD_COMM_WORLD, color, inter_win%rank_world, inter_win%COMM_INTER_NODE, mpi_err)
    
        ! Determine the rank in inter_node_comm (only for participating ranks)
        if (color /= MPI_UNDEFINED) then
            call MPI_Comm_rank(inter_win%COMM_INTER_NODE, inter_win%rank_inter_node, mpi_err)
        else
            inter_win%rank_inter_node = MPI_UNDEFINED
        end if
        !all_reduce summ of n_local => shared_size  

        ! node_id =  rank of (rank == 0 in the intra-node communicator) in the inter-node communicator
        ! broadcast node_id to all ranks in the intra-node communicator
        inter_win%node_id = inter_win%rank_intra_node
        call MPI_Bcast(inter_win%node_id, 1, MPI_INTEGER, 0, inter_win%COMM_INTRA_NODE, mpi_err)

        allocate(inter_win%ranks_mapping(3, inter_win%size_world))
        inter_win%ranks_mapping(1:3,:) = 0
        inter_win%ranks_mapping(1, inter_win%rank_world+1) = inter_win%rank_intra_node
        inter_win%ranks_mapping(2, inter_win%rank_world+1) = inter_win%node_id
        inter_win%ranks_mapping(3, inter_win%rank_world+1) = n_local

        ! allreduce ranks_mapping
        call MPI_ALLREDUCE(MPI_IN_PLACE, inter_win%ranks_mapping, &
                   inter_win%size_world*3, MPI_INTEGER, MPI_SUM, &
                   spmd_comm_world, mpi_err)

        call MPI_ALLREDUCE(n_local, inter_win%shared_size, 1, MPI_INTEGER, MPI_SUM, spmd_comm_world, mpi_err)
        inter_win%shared_size = inter_win%shared_size * disp_unit

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
  !        call MPI_Win_shared_query(win, 0, shared_size, disp_unit, shared_data)

       end if

       ! Convert base pointer to a usable Fortran pointer
       call c_f_pointer(inter_win%shared_base, inter_win%shared_data, [inter_win%shared_size / disp_unit]) 


       ibegin = 1_MPI_ADDRESS_KIND
       allocate(inter_win%addresses(inter_win%size_world+1))  
       inter_win%addresses(1) = ibegin

       do i = 1, inter_win%size_world
         ibegin = ibegin + inter_win%ranks_mapping(3,i) ! nlocal 
         inter_win%addresses(i+1) = ibegin
       end do

    end subroutine

    subroutine inter_window_update(inter_win, n_local, local_data)
        use mpi
        use iso_c_binding
        implicit none
! input arguments
        type(inter_win_), intent(inout) :: inter_win
        integer, intent(in) :: n_local
        double precision, dimension(:), intent(in) :: local_data
! local variables
        integer :: mpi_err
        integer(MPI_ADDRESS_KIND) :: i, ibegin,iend
        ! lock may not be needed, because no overlap between ranks 
        !call MPI_Win_lock_all(MPI_MODE_NOCHECK, inter_win%win, mpi_err)
        ! Barrier
        call MPI_Barrier(inter_win%COMM_INTRA_NODE, mpi_err)
        ! Update shared memory
        ibegin = inter_win%addresses(inter_win%rank_world+1)
        iend = ibegin + n_local - 1

        do i = ibegin, iend
            inter_win%shared_data(i) = local_data(i - ibegin + 1)
        end do

        !synchronize
        call MPI_Win_sync(inter_win%win, mpi_err)

    end subroutine inter_window_update

    subroutine inter_window_close(inter_win)
        use mpi
        use iso_c_binding
        implicit none
! input arguments
        type(inter_win_), intent(inout) :: inter_win
! local variables
        integer :: mpi_err

        call MPI_Win_free(inter_win%win, mpi_err)
        if(inter_win%rank_intra_node == 0) then
            deallocate(inter_win%ranks_mapping)
            deallocate(inter_win%addresses)
        end if
        if(inter_win%COMM_INTER_NODE /= MPI_COMM_NULL) then
            call MPI_Comm_free(inter_win%COMM_INTER_NODE, mpi_err)
        end if

        if(inter_win%COMM_INTRA_NODE /= MPI_COMM_NULL) then
            call MPI_Comm_free(inter_win%COMM_INTRA_NODE, mpi_err)
        end if
    end subroutine inter_window_close
end module


program main
    use mpi
    use inter_node_window
    implicit none

    type(inter_win_) :: inter_win
    integer :: spmd_comm_world, n_local, mpi_err
    integer :: i
    integer(MPI_ADDRESS_KIND) :: ibegin,iend,j
    integer :: rank
    double precision, dimension(:), allocatable :: local_data

    call MPI_Init(mpi_err)
    call MPI_Comm_dup(MPI_COMM_WORLD, spmd_comm_world, mpi_err)
    call MPI_Comm_rank(spmd_comm_world, rank, mpi_err)
    ! Initialize local data
    n_local = 10 + rank
    allocate(local_data(n_local))
    local_data = (/ (i, i = 1, n_local) /)

    ! Open inter-node window
    call inter_window_open(inter_win, n_local, spmd_comm_world)

    call inter_window_update(inter_win,n_local, local_data)
    ! Finalize MPI
    !size of world
    if(rank == 1) then
        do i = 1,inter_win%size_world
            ibegin = inter_win%addresses(i)
            iend = inter_win%addresses(i+1) - 1
            write(*,*) 'rank = ', i, 'ibegin = ', ibegin, 'iend = ', iend
            do j = ibegin, iend
                write(*,*) '             shared_data(', j, ') = ', inter_win%shared_data(j)
            enddo
        end do
    end if
    CALL MPI_Barrier(MPI_COMM_WORLD, mpi_err)
    call inter_window_close(inter_win)
    call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
    call MPI_Comm_free(spmd_comm_world, mpi_err)
    call MPI_Finalize(mpi_err)
end program main
