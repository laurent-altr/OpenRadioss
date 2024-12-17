!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      module detach_node_mod
#include "my_real.inc"
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
       !\brief This subroutine detaches a node from a list of shells
        subroutine detach_node_from_shell(nodes, node_id ,elements,shell_list,list_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE connectivity_mod 
          USE nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
            type(connectivity_), intent(inout) :: elements !< connectivity of elements
            integer, intent(in) :: node_id                 !< id of the node to detach  
            integer, intent(in) :: list_size               !< size of the shell list 
            integer, intent(in) :: shell_list(list_size)   !< list of local ids of shells to detach from the node 
! ---------------------------------------------------------------------------------------------------------------------- 
!                                                   Local variables
! ---------------------------------------------------------------------------------------------------------------------- 
            integer :: i,j 
            integer :: new_uid
            integer :: old_uid
            integer :: new_local_id
            integer :: numnod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            new_uid = nodes%max_uid + 1 
            nodes%max_uid = new_uid
            old_uid = nodes%itab(node_id)
            new_local_id = nodes%numnod + 1
            do i = 1, list_size
                do j = 1,4
                    if(elements%shells%nodes(j,shell_list(i)) == node_id) then
                        elements%shells%nodes(j,shell_list(i)) = new_local_id
                        elements%shell%ixc(j+1,shell_list(i)) = new_local_id
                    end if
                enddo
            end do
            numnod = nodes%numnod
            call extend_nodal_arrays(nodes,numnod+1)
            do i = 1, numnod
              if(nodes%itab(i) == old_uid) then
                  nodes%X(1,numnod+1) = nodes%X(1,i)
                  nodes%X(2,numnod+1) = nodes%X(2,i)
                  nodes%X(3,numnod+1) = nodes%X(3,i)
                  nodes%V(1,numnod+1) = nodes%V(1,i)
                  nodes%V(2,numnod+1) = nodes%V(2,i)
                  nodes%V(3,numnod+1) = nodes%V(3,i)
                  nodes%A(1,numnod+1) = nodes%A(1,i)
                  nodes%A(2,numnod+1) = nodes%A(2,i)
                  nodes%A(3,numnod+1) = nodes%A(3,i)
                  if(nodes%iroddl > 0) then
                    nodes%AR(1:3,numnod+1) = nodes%AR(1:3,i)
                    nodes%IN(numnod+1) = nodes%IN(i)
                  endif
                  nodes%MS(numnod+1) = nodes%MS(i) / 2.0D0 !< to be reviewed
                  nodes%MS(i) = nodes%MS(i) / 2.0D0 !< to be reviewed
                  nodes%STIFN(numnod+1) = nodes%STIFN(i)
                  nodes%VISCN(numnod+1) = nodes%VISCN(i)
                  nodes%WEIGHT(numnod+1) = 1
                  nodes%WEIGHT_MD(numnod+1) = nodes%WEIGHT_MD(i)
                  nodes%MS0(numnod+1) = nodes%MS0(i) / 2.0D0 !< to be reviewed
                  nodes%ITABM1(numnod+1) = new_uid                                  
                  nodes%ITABM1(2*numnod+2) = i
                  nodes%ICODT(numnod+1) = 0
                  nodes%ICODE(numnod+1) = nodes%ICODE(i)
                endif
            end do
            nodes%itab(numnod+1) = new_uid
            nodes%ITABM1(numnod+2:2*(numnod+1)) = nodes%ITABM1(numnod+1:2*numnod)


        end subroutine detach_node_from_shell 

      end module detach_node 
