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

      module ghost_shells_mod

        type :: spmd_buffer_type
          integer, dimension(:), allocatable :: sendbuf
          integer, dimension(:), allocatable :: recvbuf
          integer :: send_request
          integer:: recv_request
        end type spmd_buffer_type


        interface
           !call build_reverse_connectivity(element%shell,4*nb_shells,mask,reverse_connectivity)

            function build_reverse_connectivity(shells,nb_shells,mask,nspmd) result(c) bind(C,name="cpp_build_reverse_connectivity")
                use iso_c_binding
                implicit none
                integer(c_int), intent(in), value :: nspmd
                integer(c_int), intent(in), value :: nb_shells
                integer(c_int), intent(in) :: shells(4,nb_shells)
                integer(c_int), intent(in) :: mask(nspmd,*)
                type(c_ptr) :: c
            end function build_reverse_connectivity
    
            subroutine destroy_reverse_connectivity(reverse_connectivity) bind(C,name="cpp_destroy_reverse_connectivity")
                use iso_c_binding
                implicit none
                type(c_ptr), value :: reverse_connectivity
            end subroutine destroy_reverse_connectivity

            function get_shells_list(reverse_connectivity,p,n) result(cpp_ptr) bind(C,name="cpp_get_shells_list")
                use iso_c_binding
                implicit none
                type(c_ptr), intent(in) :: reverse_connectivity
                type(c_ptr) :: cpp_ptr
                integer(c_int), intent(in) :: p
                integer(c_int), intent(inout) :: n
            end function get_shells_list


        end interface

      contains
        ! \brief initializes the ghost shells
        subroutine init_ghost_shells(nodes, element,ispmd,nspmd,iad_node,sfr_node,fr_node)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use iso_c_binding
          use nodal_arrays_mod
          use connectivity_mod
          use spmd_mod
          use umap_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
          integer, intent(in) :: ispmd !< rank of the current process
          integer, intent(in) :: nspmd !< number of processes in the current MPI communicator
          integer, intent(in) :: iad_node(2,nspmd+1) !< index in bondary nodes:   J=IAD_NODE(2,I),IAD_NODE(1,I+1)-1
          integer, intent(in) :: sfr_node  !< nb nodes in the boundary
          integer, intent(in) :: fr_node(sfr_node) !<                               node = fr_node(J)
          type(nodal_arrays_) :: nodes !< nodal arrays
          type(connectivity_) :: element !< connectivity arrays
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,p,n
          integer :: ierr
          type(c_ptr) :: reverse_connectivity
          type(c_ptr) :: cpp_ptr
          integer(c_int), pointer :: list_of_shells(:)
          integer :: nb_shells !< number of shells
          integer :: numnodes !< number of nodes
          integer, dimension(:,:), allocatable :: mask!< mask for the nodes
          integer :: buffer_size_out(nspmd),buffer_size_in(nspmd)
          integer :: local_id
          type(spmd_buffer_type), dimension(nspmd) :: spmd_buffer
          integer :: TAG
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          TAG = 1000
          numnodes = nodes%numnod
          nb_shells = size(element%shell%ixc,1)
          allocate(mask(numnodes,nspmd))
          mask = 0

          do p = 1, nspmd
            do j = iad_node(2,p), iad_node(1,p+1)-1
              mask(p,fr_node(j)) = 1
            enddo
          enddo

          ! For each node with mask == 1, we identify the corresponding shells from 1 to nb_shells
          reverse_connectivity = build_reverse_connectivity(element%shell%nodes,nb_shells,mask,nspmd)
          deallocate(mask)

          ! count the number of shell to be exchanged for each processor
          allocate(element%ghost_shell%list_of_shells(nspmd))
          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            cpp_ptr= get_shells_list(reverse_connectivity,p,n)
            call c_f_pointer(cpp_ptr, list_of_shells,[n])
            buffer_size_out(p) = n
            allocate(spmd_buffer(p)%sendbuf(4*n))
            allocate(element%ghost_shell%list_of_shells(p)%index(n))
            if(n > 0) then
              ! copy the list of shells to be exchanged
              element%ghost_shell%list_of_shells(p)%index(1:n) = list_of_shells(1:n)
            endif
          enddo
          ! Call MPI_Alltoall to exchange the number of shells to be exchanged
          call MPI_Alltoall(buffer_size_out,nspmd,MPI_INTEGER,buffer_size_in,nspmd,MPI_INTEGER,SPMD_COMM_WORLD,ierr)


          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            ! mpi Irecv to receive the data
            allocate(spmd_buffer(p)%recvbuf(4*buffer_size_in(p)))
            n = buffer_size_in(p)
            if( n > 0 ) then
              call MPI_Irecv(spmd_buffer(p)%recvbuf,4*n,MPI_INTEGER,p-1,TAG,SPMD_COMM_WORLD,spmd_buffer(p)%recv_request,ierr)
              do i = 1, n
                element%ghost_shell%nodes(1,i) = spmd_buffer(p)%recvbuf(1+4*(i-1))
                element%ghost_shell%nodes(2,i) = spmd_buffer(p)%recvbuf(2+4*(i-1))
                element%ghost_shell%nodes(3,i) = spmd_buffer(p)%recvbuf(3+4*(i-1))
                element%ghost_shell%nodes(4,i) = spmd_buffer(p)%recvbuf(4+4*(i-1))
                ! convert back user id to local id
                do j = 1,4
                  local_id = get_local_node_id(nodes,element%ghost_shell%nodes(j,i))
                  if (local_id > 0) then
                    element%ghost_shell%nodes(j,i) = local_id
                  else
                    element%ghost_shell%nodes(j,i) = -element%ghost_shell%nodes(j,i) ! negative id for nodes unknown on this processor
                  endif
                enddo

              enddo
            endif

            ! mpi Isend to send the data
            cpp_ptr =get_shells_list(reverse_connectivity,p,n)
            if( n > 0 ) then
              call c_f_pointer(cpp_ptr, list_of_shells,[n])
              do i = 1, n
                j = list_of_shells(i)
                spmd_buffer(p)%sendbuf(1+4*(i-1)) = nodes%itab(element%shell%nodes(1,j))
                spmd_buffer(p)%sendbuf(2+4*(i-1)) = nodes%itab(element%shell%nodes(2,j))
                spmd_buffer(p)%sendbuf(3+4*(i-1)) = nodes%itab(element%shell%nodes(3,j))
                spmd_buffer(p)%sendbuf(4+4*(i-1)) = nodes%itab(element%shell%nodes(4,j))
              enddo
              call MPI_Isend(spmd_buffer(p)%sendbuf,4*n,MPI_INTEGER,p-1,TAG,SPMD_COMM_WORLD,spmd_buffer(p)%send_request,ierr)
            endif
          enddo



          ! Wait for all the sends to complete
          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            cpp_ptr = get_shells_list(reverse_connectivity,p,n)
            if(n > 0) then
              call MPI_Wait(spmd_buffer(p)%send_request,ierr)
              do i = 1, n
                element%ghost_shell%nodes(1,i) = spmd_buffer(p)%recvbuf(1+4*(i-1))
                element%ghost_shell%nodes(2,i) = spmd_buffer(p)%recvbuf(2+4*(i-1))
                element%ghost_shell%nodes(3,i) = spmd_buffer(p)%recvbuf(3+4*(i-1))
                element%ghost_shell%nodes(4,i) = spmd_buffer(p)%recvbuf(4+4*(i-1))
                ! convert back user id to local id
                do j = 1,4
                  local_id = get_local_node_id(nodes,element%ghost_shell%nodes(j,i))
                  if (local_id > 0) then
                    element%ghost_shell%nodes(j,i) = local_id
                  else
                    element%ghost_shell%nodes(j,i) = -element%ghost_shell%nodes(j,i) ! negative id for nodes unknown on this processor
                  endif
                enddo
              enddo
              deallocate(spmd_buffer(p)%sendbuf)
            endif
          enddo

          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            n = buffer_size_in(p)
            if( n > 0 ) then
              call MPI_Wait(spmd_buffer(p)%recv_request,ierr)
              deallocate(spmd_buffer(p)%recvbuf)
            endif
          enddo

          call destroy_reverse_connectivity(reverse_connectivity)
        end subroutine

      end module ghost_shells_mod


