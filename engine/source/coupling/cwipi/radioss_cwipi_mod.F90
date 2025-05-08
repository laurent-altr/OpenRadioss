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
      module radioss_cwipi_mod
        implicit none
        integer, parameter :: CWIPI_SEND = 1
        integer, parameter :: CWIPI_RECV = 2
        integer, parameter :: CWIPI_COORDINATE = 1
        integer, parameter :: CWIPI_VELOCITY = 2
        integer, parameter :: CWIPI_ACCELERATION = 3
        integer, parameter :: CWIPI_FORCE = 4
        integer, parameter :: CWIPI_PRESSURE = 5
        integer, parameter :: NOTHING = 0

        !/CWIPI/SurfID/in/type

        type cwipi_field_
          integer :: surface_id
          integer :: quantity ! 1: position, 2: velocity, 3: acceleration, 4: force
          integer :: n_faces
          integer :: numnod !< number of node of that interface
          integer, dimension(:), allocatable :: connectIndex
          integer, dimension(:), allocatable :: connec
          integer, dimension(:), allocatable :: node_id
          double precision, dimension(:), allocatable :: buffer
        end type cwipi_field_

        type cwipi_
          integer :: i_part
          type(cwipi_field_) :: send
          type(cwipi_field_) :: recv
        end type cwipi_


      contains

       ! surfaces are made of 4 nodes. In case of triangle, a node is duplicated
       ! this subroutine counts the number of unique nodes in a surface
function make_unique(arr) result(n_unique)
  implicit none
  integer, intent(inout) :: arr(4)
  integer :: n_unique
  integer :: temp(4)
  integer :: i, j
  logical :: is_new

  n_unique = 0

  ! Identify unique values and store them in temp
  do i = 1, 4
    if (arr(i) < 0) cycle
    is_new = .true.
    do j = 1, n_unique
      if (arr(i) == temp(j)) then
        is_new = .false.
        exit
      end if
    end do
    if (is_new) then
      n_unique = n_unique + 1
      temp(n_unique) = arr(i)
    end if
  end do

  ! Fill arr with the compacted unique values and -1
  do i = 1, n_unique
    arr(i) = temp(i)
  end do
  do i = n_unique + 1, 4
    arr(i) = -1
  end do

end function make_unique



        subroutine parse_cwipi_string(input_string, cwipi)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=*), intent(in) :: input_string
          type(cwipi_), intent(inout) :: cwipi
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: first_slash, second_slash, third_slash
          character(len=20) :: surf_id_str, dir_str, quant_str
          integer :: read_status
          integer :: surf_id, direction, quantity
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------


          write(6,*) 'parse_cwipi_string: input_string:', input_string

          ! Check prefix
          if (len(input_string) < 7) then
            write(6,*) 'parse_cwipi_string: input_string too short'
            return
          end if
          if (input_string(1:7) /= '/CWIPI/') then
            write(6,*) 'parse_cwipi_string: input_string does not start with /CWIPI/'
            return
          end if

          ! Find slash positions
          first_slash = 7  ! After "/CWIPI/"
          second_slash = index(input_string(first_slash+1:), '/') + first_slash
          if (second_slash <= first_slash) then
            write(6,*) 'parse_cwipi_string: second slash not found'
            return
          end if

          third_slash = index(input_string(second_slash+1:), '/') + second_slash
          if (third_slash <= second_slash) then
            write(6,*) 'parse_cwipi_string: third slash not found'
            return
          end if

          ! Extract parts
          surf_id_str = input_string(first_slash+1:second_slash-1)
          dir_str = input_string(second_slash+1:third_slash-1)
          quant_str = input_string(third_slash+1:)

          ! Parse surfId
          read(surf_id_str, *, iostat=read_status) surf_id
          if (read_status /= 0) then
            write(6,*) 'parse_cwipi_string: invalid surf_id:', surf_id_str
            return
          end if


          ! Convert quantity string to constant
          select case (trim(quant_str))
           case ('COORDINATE')
            quantity = CWIPI_COORDINATE
           case ('VELOCITY')
            quantity = CWIPI_VELOCITY
           case ('ACCELERATION')
            quantity = CWIPI_ACCELERATION
           case ('FORCE')
            quantity = CWIPI_FORCE
           case ('PRESSURE')
            quantity = CWIPI_PRESSURE
           case default
            quantity = NOTHING  ! Set default quantity to NOTHING
            write(6,*) 'parse_cwipi_string: invalid quantity:', quant_str
          end select

          ! Convert direction string to constant
          select case (trim(dir_str))
           case ('IN')
            cwipi%recv%surface_id = surf_id
            cwipi%recv%quantity = quantity
           case ('OUT')
            cwipi%send%surface_id = surf_id
            cwipi%send%quantity = quantity
           case default
            direction = NOTHING  ! Set default direction to NOTHING
          end select
        end subroutine parse_cwipi_string

        subroutine cwipi_init_field(field, surf,  nodes)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
!----------------------------------------------------------------------------------------------------------------------
            use GROUPDEF_MOD, only: surf_
            use nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(cwipi_field_), intent(inout) :: field !< CWIPI field structure
          type(surf_), intent(in) :: surf !< Array of surfaces
          type(nodal_arrays_), intent(in) :: nodes !< Nodal arrays
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,n,counter
          integer, dimension(:), allocatable :: index
          integer :: nb_unique_nodes
          integer :: next_node
          integer :: tmp(4)
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(index(nodes%numnod))
          allocate(field%connectIndex(surf%NSEG+1))
          field%connectIndex = 0
          field%connectIndex(1) = 0
          allocate(field%connec(surf%NSEG*4))
          field%connec = 0
          allocate(field%node_id(surf%NSEG*4))
          field%node_id = 0

          write(6,*) "surf%nodes size:", size(surf%nodes)
          write(6,*) "surf%NSEG size:", surf%NSEG
          write(6,*) "surf%elem size", size(surf%elem)
          write(6,*) "surf%proc size:", size(surf%proc)
          write(6,*) "surf%eltyp size:", size(surf%eltyp)
          write(6,*) "surf%type",surf%type
          call flush(6)
          index = 0
          counter = 0
          next_node = 0
          do i = 1, surf%nseg
            field%connectIndex(i+1) = field%connectIndex(i) 
            ! check if it's a triangle = two surf%nodes(:,i) are the same
            tmp(1) = surf%nodes(i,1)
            tmp(2) = surf%nodes(i,2)
            tmp(3) = surf%nodes(i,3)
            tmp(4) = surf%nodes(i,4)
            nb_unique_nodes = make_unique(tmp)
            field%connectIndex(i+1) = field%connectIndex(i+1) + nb_unique_nodes 
            do j = 1, nb_unique_nodes
                if(tmp(j) < 0) then
                    write(6,*) 'Error in surf%nodes', tmp(j)
                    cycle
                endif
                if(tmp(j) > nodes%numnod) then
                  write(6,*) 'Error in surf%nodes', tmp(j), nodes%numnod
                  cycle
                end if
                n = tmp(j)
                if(index(n) == 0) then
                  counter = counter + 1
                  index(n) = counter
                  field%node_id(counter) = n
                end if
                next_node = next_node + 1

                ! next_node should be equal to field%vtx_id(i) + j - 1 ?

                if(next_node .ne. field%connectIndex(i) + j - 1) then
                  write(6,*) 'Error in field%connectIndex?', field%connectIndex(i)+j-1, next_node
                end if
                field%connec(next_node) = index(n) -1
            enddo
          enddo

          allocate(field%buffer(3*next_node)) 
          field%numnod = counter

        end subroutine cwipi_init_field

        subroutine cwipi_init(cwipi, surf, nsurf, nodes)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
#ifdef CWIPI
          use cwipi
#endif
          use GROUPDEF_MOD, only: surf_
          use nodal_arrays_mod
          implicit none
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nsurf !< Number of surfaces
          type(cwipi_), intent(inout) :: cwipi !< CWIPI structure
          type(surf_), intent(in) :: surf(nsurf) !< Array of surfaces
          type(nodal_arrays_), intent(in) :: nodes !< Nodal arrays
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i 
          integer :: surface_id  !< Variable to hold surface ID
          double precision :: tolerance !
          double precision, dimension(:), allocatable :: coords
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          do i = 1,nsurf
            if(cwipi%send%surface_id == surf(i)%id) then
              cwipi%send%n_faces = surf(i)%NSEG
              cwipi%send%surface_id= i 
              call cwipi_init_field(cwipi%send, surf(i), nodes)
              write(6,*) 'cwipi_init: send surface_id:', cwipi%send%surface_id,"nodes=",cwipi%send%numnod
            end if
            if(cwipi%recv%surface_id == surf(i)%id) then
              cwipi%recv%n_faces = surf(i)%NSEG
              cwipi%recv%surface_id = i
              call cwipi_init_field(cwipi%recv, surf(i) , nodes)
              write(6,*) 'cwipi_init: recv surface_id:', cwipi%recv%surface_id,"nodes=",cwipi%recv%numnod
            end if
          enddo

          allocate(coords(3*cwipi%send%numnod))

          do i = 1, cwipi%send%numnod
            coords(3*i-2) = nodes%X(1,cwipi%send%node_id(i))
            coords(3*i-1) = nodes%X(2,cwipi%send%node_id(i))
            coords(3*i)   = nodes%X(3,cwipi%send%node_id(i))
          enddo


#ifdef CWIPI
! CWIPI INITIALIZAITON 
      call cwipi_create_coupling_f("r2r", &
      cwipi_cpl_parallel_with_part,&
      "aspi_reel_solver", &
      2,     & ! Dimension des entites geometriques
      tolerance, & ! Tolerance geometrique
      cwipi_static_mesh, &
      cwipi_solver_cell_vertex, &
      1, &
      "Ensight Gold",&
      "text")

      call cwipi_define_mesh_f("r2r", &
      cwipi%send%numnod, &
      cwipi%send%n_faces, &
      coords, &
      cwipi%send%connectIndex, &
      cwipi%send%connec)

      call cwipi_dump_appli_properties_f()
      call cwipi_locate("r2r")
#endif 

        deallocate(coords)
        end subroutine cwipi_init


        subroutine cwipi_out(cwipi,  nodes, t)

#ifdef CWIPI
          use cwipi
#endif
          use nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(cwipi_), intent(inout) :: cwipi !< CWIPI structure
          type(nodal_arrays_), intent(in) :: nodes !< Nodal arrays
          double precision, intent(in) :: t !< Time
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j
          integer :: ierr
          integer :: rq
          character (len = 20) :: fieldName
          double precision :: time

! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          time = t
          ! Send the data to the other side
          if(.not.allocated(cwipi%send%buffer)) then
            allocate(cwipi%send%buffer(3*cwipi%send%numnod))
          end if

          do i = 1, cwipi%send%numnod
            if(cwipi%send%quantity == CWIPI_COORDINATE) then
              cwipi%send%buffer(3*i-2) = nodes%X(1,cwipi%send%node_id(i)) 
              cwipi%send%buffer(3*i-1) = nodes%X(2,cwipi%send%node_id(i)) 
              cwipi%send%buffer(3*i)   = nodes%X(3,cwipi%send%node_id(i)) 
              fieldName = "COORDINATE"
            else if(cwipi%send%quantity == CWIPI_VELOCITY) then
              cwipi%send%buffer(3*i-2) = nodes%V(1,cwipi%send%node_id(i))
              cwipi%send%buffer(3*i-1) = nodes%V(2,cwipi%send%node_id(i))
              cwipi%send%buffer(3*i)   = nodes%V(3,cwipi%send%node_id(i))
              fieldName = "VELOCITY"
            else if(cwipi%send%quantity == CWIPI_ACCELERATION) then
              cwipi%send%buffer(3*i-2) = nodes%a(1,cwipi%send%node_id(i))
              cwipi%send%buffer(3*i-1) = nodes%a(2,cwipi%send%node_id(i))
              cwipi%send%buffer(3*i)   = nodes%a(3,cwipi%send%node_id(i))
              fieldName = "ACCELERATION"
            else if(cwipi%send%quantity == CWIPI_FORCE) then
              cwipi%send%buffer(3*i-2) = nodes%F(1,cwipi%send%node_id(i))
              cwipi%send%buffer(3*i-1) = nodes%F(2,cwipi%send%node_id(i))
              cwipi%send%buffer(3*i)   = nodes%F(3,cwipi%send%node_id(i))
              fieldName = "FORCE"
            endif
          end do


#ifdef CWIPI
          call cwipi_update_location_f("r2r")
          call cwipi_locate_f("r2r")
          call cwipi_issend_f("y2y2", &
                            "exchange", &
                            0, &
                            3, &
                            1, &
                            time, &
                            fieldName, &
                            cwipi%send%buffer, &
                            rq)
            call cwipi_wait_issend_f("y2y2",rq)
#endif
  
        end subroutine cwipi_out


        subroutine cwipi_in(cwipi, nodes, t)
#ifdef CWIPI
          use cwipi
#endif
          use nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(cwipi_), intent(inout) :: cwipi !< CWIPI structure
          type(nodal_arrays_), intent(inout) :: nodes !< Nodal arrays
          double precision :: t !< Time
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j
          integer :: ierr
          integer :: rq
          integer :: nNotLocatedPoints
          integer :: status
          character (len = 20) :: fieldName
          double precision :: time
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body     
! ----------------------------------------------------------------------------------------------------------------------
          if(.not.allocated(cwipi%recv%buffer)) then
            allocate(cwipi%recv%buffer(3*cwipi%recv%numnod))
          end if
          time = t

          if(cwipi%recv%quantity == CWIPI_COORDINATE) then
            fieldName = "COORDINATE"
          else if(cwipi%recv%quantity == CWIPI_VELOCITY) then
            fieldName = "VELOCITY"
          else if(cwipi%recv%quantity == CWIPI_ACCELERATION) then
            fieldName = "ACCELERATION"
          else if(cwipi%recv%quantity == CWIPI_FORCE) then
            fieldName = "FORCE"
          endif

          ! Receive the data from the other side
#ifdef CWIPI
          CALL cwipi_receive_f('r2r', &
                      'exchange', &
                      3, &
                      1, &
                      time, &
                      recievingField, &
                      cwipi%recv%buffer, &
                      nNotLocatedPoints, &
                      status)
#endif
          do i = 1, cwipi%recv%numnod
            if(cwipi%recv%quantity == CWIPI_COORDINATE) then
              nodes%X(1,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i-2)
              nodes%X(2,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i-1)
              nodes%X(3,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i)
            else if(cwipi%recv%quantity == CWIPI_VELOCITY) then
              nodes%V(1,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i-2)
              nodes%V(2,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i-1)
              nodes%V(3,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i)
            else if(cwipi%recv%quantity == CWIPI_ACCELERATION) then
              nodes%A(1,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i-2)
              nodes%A(2,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i-1)
              nodes%A(3,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i)
            else if(cwipi%recv%quantity == CWIPI_FORCE) then
              nodes%F(1,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i-2)
              nodes%F(2,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i-1)
              nodes%F(3,cwipi%recv%node_id(i)) = cwipi%recv%buffer(3*i)
            endif
          end do
        end subroutine cwipi_in



        subroutine cwipi_free(cwipi)
#ifdef CWIPI
          use cwipi
#endif
            implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(cwipi_), intent(inout) :: cwipi !< CWIPI structure

          if(allocated(cwipi%send%buffer)) deallocate(cwipi%send%buffer)
          if(allocated(cwipi%send%connec)) deallocate(cwipi%send%connec)
          if(allocated(cwipi%send%node_id)) deallocate(cwipi%send%node_id)
          if(allocated(cwipi%send%connectIndex)) deallocate(cwipi%send%connectIndex)
          if(allocated(cwipi%recv%buffer)) deallocate(cwipi%recv%buffer)
          if(allocated(cwipi%recv%connec)) deallocate(cwipi%recv%connec)
          if(allocated(cwipi%recv%node_id)) deallocate(cwipi%recv%node_id)
          if(allocated(cwipi%recv%connectIndex)) deallocate(cwipi%recv%connectIndex)
#ifdef CWIPI
          call cwipi_delete_coupling_f("r2r")
          call cwipi_finalize()
#endif
        end subroutine cwipi_free
! ----------------------------------------------------------------------------------------------------------------------

      end module radioss_cwipi_mod





