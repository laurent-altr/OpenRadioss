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
        module update_patch_mod
        ! intersects is private, update_patch is public
        public :: intersects
        public :: update_patch

        contains
        logical function intersects(a, b) result(intersect)
            implicit none
            double precision, dimension(3,2), intent(in) :: a, b
            
            ! Check if boxes are valid (optional, can be removed if all inputs are valid)
            intersect = .true.
            if (a(1,1) > b(1,2) .or. a(1,2) < b(1,1) .or. &  ! X-axis check
                a(2,1) > b(2,2) .or. a(2,2) < b(2,1) .or. &  ! Y-axis check
                a(3,1) > b(3,2) .or. a(3,2) < b(3,1)) then    ! Z-axis check
                intersect = .false.
            end if
    
        end function intersects

        subroutine update_patch(nodes,inter_struct, inter_comm, stfn,nsn)
        use intbufdef_mod
        use nodal_arrays_mod
        USE INTER_STRUCT_MOD
        implicit none
#include "spmd.inc"
#include "my_real.inc"
! dummy arguments
        type(inter_struct_type) :: inter_struct
        type(nodal_arrays_), intent(in) :: nodes
        integer :: nsn
        my_real :: stfn(nsn)
        integer :: inter_comm !< MPI communicator
! local variables    

        integer :: i, j, k
        integer :: numnod
        double precision :: dx, dy, dz
        double precision :: x_min, y_min, z_min
        double precision :: x_max, y_max, z_max
        double precision :: xx,yy,zz
        integer :: rank,p
        double precision, dimension(:), allocatable :: buffer_in
        double precision, dimension(:), allocatable :: buffer_out
        integer :: size_in, size_out, err
        integer, dimension(:), allocatable :: counter, displs
        integer :: my_rank, comm_size

        rank = inter_struct%win%rank_inter + 1
        numnod = nodes%numnod
        x_min = HUGE(1.0d0)
        y_min = HUGE(1.0d0)
        z_min = HUGE(1.0d0)
        x_max = -HUGE(1.0d0)
        y_max = -HUGE(1.0d0)
        z_max = -HUGE(1.0d0)

        ! initialize the bounding box
        do i = 1, inter_struct%win%patchs(rank)%nb_clusters                           
           inter_struct%win%patchs(rank)%clusters(i)%bounding_box(1,1) = x_min
           inter_struct%win%patchs(rank)%clusters(i)%bounding_box(2,1) = y_min
           inter_struct%win%patchs(rank)%clusters(i)%bounding_box(3,1) = z_min
           inter_struct%win%patchs(rank)%clusters(i)%bounding_box(1,2) = x_max
           inter_struct%win%patchs(rank)%clusters(i)%bounding_box(2,2) = y_max
           inter_struct%win%patchs(rank)%clusters(i)%bounding_box(3,2) = z_max
        enddo

        size_in = 6 * inter_struct%win%patchs(rank)%nb_clusters
        allocate(buffer_in(size_in))
        do i = 1,inter_struct%win%patchs(rank)%nb_clusters
            do j = 1, inter_struct%win%patchs(rank)%clusters(i)%numnod
                k = inter_struct%win%patchs(rank)%clusters(i)%index_in_nsv(j)
                if (stfn(k) == 0.0d0) cycle
                k = inter_struct%win%patchs(rank)%clusters(i)%index_in_x(j)
                xx = nodes%X(1,k)
                yy = nodes%X(2,k)
                zz = nodes%X(3,k)
                if (xx < inter_struct%win%patchs(rank)%clusters(i)%bounding_box(1,1)) then
                    inter_struct%win%patchs(rank)%clusters(i)%bounding_box(1,1) = xx
                endif
                if (xx > inter_struct%win%patchs(rank)%clusters(i)%bounding_box(1,2)) then
                    inter_struct%win%patchs(rank)%clusters(i)%bounding_box(1,2) = xx
                endif
                if (yy < inter_struct%win%patchs(rank)%clusters(i)%bounding_box(2,1)) then
                    inter_struct%win%patchs(rank)%clusters(i)%bounding_box(2,1) = yy
                endif
                if (yy > inter_struct%win%patchs(rank)%clusters(i)%bounding_box(2,2)) then
                    inter_struct%win%patchs(rank)%clusters(i)%bounding_box(2,2) = yy
                endif
                if (zz < inter_struct%win%patchs(rank)%clusters(i)%bounding_box(3,1)) then
                    inter_struct%win%patchs(rank)%clusters(i)%bounding_box(3,1) = zz
                endif
                if (zz > inter_struct%win%patchs(rank)%clusters(i)%bounding_box(3,2)) then
                    inter_struct%win%patchs(rank)%clusters(i)%bounding_box(3,2) = zz
                endif
            enddo
            ! fill buffer_in with the 6 values
            buffer_in((1+(i-1)*6):(i*6)) = reshape(inter_struct%win%patchs(rank)%clusters(i)%bounding_box, [6]) 

        enddo

        allocate(counter(inter_struct%win%size_inter))
        allocate(displs(inter_struct%win%size_inter+1))
        displs = 0
        counter = 0

        size_out = 0
        do p = 1, inter_struct%win%size_inter
            size_out = size_out + inter_struct%win%patchs(p)%nb_clusters * 6
            displs(p+1) = size_out
            counter(p) = inter_struct%win%patchs(p)%nb_clusters *6
        enddo
        allocate(buffer_out(size_out))
        ! MPI_Allgaterv the bounding boxes
!       write(6,*) 'size_out', size_out
!       write(6,*) 'size_in', size_in
!       call MPI_Comm_size(inter_comm, comm_size, err)
!       call MPI_Comm_rank(inter_comm, my_rank, err)
!       write(6,*) 'Process', my_rank, 'of', comm_size, ': Preparing for MPI_Allgatherv'
!       write(6,*) 'Process', my_rank, ': size_in =', size_in
!       write(6,*) 'Process', my_rank, ': buffer_in size =', size(buffer_in)
!       write(6,*) 'Process', my_rank, ': size_out =', size_out
!       write(6,*) 'Process', my_rank, ': buffer_out size =', size(buffer_out)
!       write(6,*) 'Process', my_rank, ': counter =', counter
!       write(6,*) 'Process', my_rank, ': displs =', displs
!       write(6,*) 'Process', my_rank, ': sum(counter) =', sum(counter)
!       call flush(6)
        call MPI_Allgatherv(buffer_in, size_in, MPI_DOUBLE_PRECISION, &
                            buffer_out, counter, displs, MPI_DOUBLE_PRECISION, &
                            inter_comm, err)
        
!        write(6,*) 'buffer_out', buffer_out


        ! fill the bounding boxes
        do p = 1, inter_struct%win%size_inter
            do i = 1, inter_struct%win%patchs(p)%nb_clusters
                ! all 6 values
                 j = displs(p) + (i-1)*6
                inter_struct%win%patchs(p)%clusters(i)%bounding_box(1,1) = buffer_out(j+1)
                inter_struct%win%patchs(p)%clusters(i)%bounding_box(2,1) = buffer_out(j+2)
                inter_struct%win%patchs(p)%clusters(i)%bounding_box(3,1) = buffer_out(j+3)
                inter_struct%win%patchs(p)%clusters(i)%bounding_box(1,2) = buffer_out(j+4)
                inter_struct%win%patchs(p)%clusters(i)%bounding_box(2,2) = buffer_out(j+5)
                inter_struct%win%patchs(p)%clusters(i)%bounding_box(3,2) = buffer_out(j+6)
            enddo
        enddo

        deallocate(buffer_in)
        deallocate(buffer_out)
        deallocate(counter)

    end subroutine 
        subroutine update_main_patch(nodes,inter_struct, msr,nmn)
        use intbufdef_mod
        use nodal_arrays_mod
        USE INTER_STRUCT_MOD
        implicit none
#include "spmd.inc"
! dummy arguments
        type(inter_struct_type) :: inter_struct
        type(nodal_arrays_), intent(in) :: nodes
        integer :: nmn
        integer :: msr(nmn)
        integer :: inter_comm !< MPI communicator
! local variables    

        integer :: i, j, k
        integer :: numnod
        double precision :: dx, dy, dz
        double precision :: x_min, y_min, z_min
        double precision :: x_max, y_max, z_max
        double precision :: xx,yy,zz
        integer :: p
        double precision, dimension(:), allocatable :: buffer_in
        double precision, dimension(:), allocatable :: buffer_out
        integer :: size_in, size_out, err
        integer, dimension(:), allocatable :: counter, displs
        integer :: my_rank, comm_size

        numnod = nodes%numnod
        x_min = HUGE(1.0d0)
        y_min = HUGE(1.0d0)
        z_min = HUGE(1.0d0)
        x_max = -HUGE(1.0d0)
        y_max = -HUGE(1.0d0)
        z_max = -HUGE(1.0d0)

        ! initialize the bounding box
        do i = 1, inter_struct%win%main_patchs%nb_clusters                           
           inter_struct%win%main_patchs%clusters(i)%bounding_box(1,1) = x_min
           inter_struct%win%main_patchs%clusters(i)%bounding_box(2,1) = y_min
           inter_struct%win%main_patchs%clusters(i)%bounding_box(3,1) = z_min
           inter_struct%win%main_patchs%clusters(i)%bounding_box(1,2) = x_max
           inter_struct%win%main_patchs%clusters(i)%bounding_box(2,2) = y_max
           inter_struct%win%main_patchs%clusters(i)%bounding_box(3,2) = z_max
        enddo

        size_in = 6 * inter_struct%win%main_patchs%nb_clusters
        allocate(buffer_in(size_in))
        do i = 1,inter_struct%win%main_patchs%nb_clusters
            do j = 1, inter_struct%win%main_patchs%clusters(i)%numnod
                k = inter_struct%win%main_patchs%clusters(i)%index_in_nsv(j)
                if (msr(k) > 0) cycle
                k = inter_struct%win%main_patchs%clusters(i)%index_in_x(j)
                xx = nodes%X(1,k)
                yy = nodes%X(2,k)
                zz = nodes%X(3,k)
                if (xx < inter_struct%win%main_patchs%clusters(i)%bounding_box(1,1)) then
                    inter_struct%win%main_patchs%clusters(i)%bounding_box(1,1) = xx
                endif
                if (xx > inter_struct%win%main_patchs%clusters(i)%bounding_box(1,2)) then
                    inter_struct%win%main_patchs%clusters(i)%bounding_box(1,2) = xx
                endif
                if (yy < inter_struct%win%main_patchs%clusters(i)%bounding_box(2,1)) then
                    inter_struct%win%main_patchs%clusters(i)%bounding_box(2,1) = yy
                endif
                if (yy > inter_struct%win%main_patchs%clusters(i)%bounding_box(2,2)) then
                    inter_struct%win%main_patchs%clusters(i)%bounding_box(2,2) = yy
                endif
                if (zz < inter_struct%win%main_patchs%clusters(i)%bounding_box(3,1)) then
                    inter_struct%win%main_patchs%clusters(i)%bounding_box(3,1) = zz
                endif
                if (zz > inter_struct%win%main_patchs%clusters(i)%bounding_box(3,2)) then
                    inter_struct%win%main_patchs%clusters(i)%bounding_box(3,2) = zz
                endif
            enddo
        enddo
    end subroutine 

    end module update_patch_mod
