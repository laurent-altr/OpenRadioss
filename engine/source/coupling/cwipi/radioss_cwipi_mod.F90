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
      module radioss_coupling_mod
        implicit none
        integer, parameter :: COUPLING_SEND = 1
        integer, parameter :: COUPLING_RECV = 2
        integer, parameter :: COUPLING_COORDINATE = 1
        integer, parameter :: COUPLING_VELOCITY = 2
        integer, parameter :: COUPLING_ACCELERATION = 3
        integer, parameter :: COUPLING_FORCE = 4
        integer, parameter :: COUPLING_PRESSURE = 5
        integer, parameter :: NOTHING = 0

        !/coupling/SurfID/in/type

        type coupling_field_
          integer :: surface_id
          integer :: quantity ! 1: position, 2: velocity, 3: acceleration, 4: force
          integer :: n_faces
          integer :: numnod !< number of node of that interface
          integer, dimension(:), allocatable :: connectIndex
          integer, dimension(:), allocatable :: connec
          integer, dimension(:), allocatable :: node_id
          double precision, dimension(:), allocatable :: buffer
          integer :: rq !< requests
        end type coupling_field_

        type coupling_
          integer :: i_part
          type(coupling_field_) :: send
          type(coupling_field_) :: recv
        end type coupling_


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



        subroutine parse_coupling_string(input_string, coupling)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=*), intent(in) :: input_string
          type(coupling_), intent(inout) :: coupling
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


          write(6,*) 'parse_coupling_string: input_string:', input_string

          ! Check prefix
          if (len(input_string) < 7) then
            write(6,*) 'parse_coupling_string: input_string too short'
            return
          end if
          if (input_string(1:7) /= '/CWIPI/') then
            write(6,*) 'parse_coupling_string: input_string does not start with /coupling/'
            return
          end if

          ! Find slash positions
          first_slash = 7  ! After "/coupling/"
          second_slash = index(input_string(first_slash+1:), '/') + first_slash
          if (second_slash <= first_slash) then
            write(6,*) 'parse_coupling_string: second slash not found'
            return
          end if

          third_slash = index(input_string(second_slash+1:), '/') + second_slash
          if (third_slash <= second_slash) then
            write(6,*) 'parse_coupling_string: third slash not found'
            return
          end if

          ! Extract parts
          surf_id_str = input_string(first_slash+1:second_slash-1)
          dir_str = input_string(second_slash+1:third_slash-1)
          quant_str = input_string(third_slash+1:)

          ! Parse surfId
          read(surf_id_str, *, iostat=read_status) surf_id
          if (read_status /= 0) then
            write(6,*) 'parse_coupling_string: invalid surf_id:', surf_id_str
            return
          end if

          write(6,*) 'quantity:', quant_str,trim(quant_str)

          ! Convert quantity string to constant
          select case (trim(quant_str))
           case ('COORDINATE')
            quantity = coupling_COORDINATE
           case ('VELOCITY')
            quantity = coupling_VELOCITY
           case ('ACCELERATION')
            quantity = coupling_ACCELERATION
           case ('FORCE')
            quantity = coupling_FORCE
           case ('PRESSURE')
            quantity = coupling_PRESSURE
           case default
            quantity = NOTHING  ! Set default quantity to NOTHING
            write(6,*) 'parse_coupling_string: invalid quantity:', quant_str
            call arret(2)
          end select

          ! Convert direction string to constant
          select case (trim(dir_str))
           case ('IN')
            coupling%recv%surface_id = surf_id
            coupling%recv%quantity = quantity
           case ('OUT')
            coupling%send%surface_id = surf_id
            coupling%send%quantity = quantity
           case default
            write(6,*) 'parse_coupling_string: invalid direction:', dir_str 
            call arret(2)
          end select

          write(6,*) trim(dir_str),'quantity:', quantity

          
        end subroutine parse_coupling_string

        subroutine coupling_init_field(field, surf,  nodes)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
!----------------------------------------------------------------------------------------------------------------------
            use GROUPDEF_MOD, only: surf_
            use nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_field_), intent(inout) :: field !< coupling field structure
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
            !write(6,*) "surf%nodes", tmp(1), tmp(2), tmp(3), tmp(4)
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


                if(next_node .ne. field%connectIndex(i) + j ) then
                  write(6,*) 'Error in field%connectIndex?', field%connectIndex(i)+j, next_node
                end if
                field%connec(next_node) = index(n)
            enddo
          enddo

          allocate(field%buffer(3*counter)) 
          field%numnod = counter

        end subroutine coupling_init_field

        subroutine coupling_init(coupling, surf, nsurf, nodes )
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
#ifdef WITH_CWIPI
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
          type(coupling_), intent(inout) :: coupling !< coupling structure
          type(surf_), intent(in) :: surf(nsurf) !< Array of surfaces
          type(nodal_arrays_), intent(in) :: nodes !< Nodal arrays
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i 
          integer :: surface_id  !< Variable to hold surface ID
          double precision :: tolerance !
          double precision, dimension(:), allocatable :: coords
          character(5) :: APPNAME
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          do i = 1,nsurf
            if(coupling%send%surface_id == surf(i)%id) then
              coupling%send%n_faces = surf(i)%NSEG
              coupling%send%surface_id= i 
              call coupling_init_field(coupling%send, surf(i), nodes)
              write(6,*) 'coupling_init: send surface_id:', coupling%send%surface_id,"nodes=",coupling%send%numnod
            end if
            if(coupling%recv%surface_id == surf(i)%id) then
              coupling%recv%n_faces = surf(i)%NSEG
              coupling%recv%surface_id = i
              call coupling_init_field(coupling%recv, surf(i) , nodes)
              write(6,*) 'coupling_init: recv surface_id:', coupling%recv%surface_id,"nodes=",coupling%recv%numnod
            end if
          enddo

          allocate(coords(3*coupling%send%numnod))

          do i = 1, coupling%send%numnod
            coords(3*i-2) = nodes%X(1,coupling%send%node_id(i))
            coords(3*i-1) = nodes%X(2,coupling%send%node_id(i))
            coords(3*i)   = nodes%X(3,coupling%send%node_id(i))
          enddo
        
#ifdef WITH_CWIPI
! coupling INITIALIZAITON 
      IF(APPLICATION_ID == 0) THEN
      APPNAME = "code2"
      call cwipi_create_coupling_f("r2r", &
      cwipi_cpl_parallel_with_part,&
      APPNAME, &
      2,     & ! Dimension des entites geometriques
      tolerance, & ! Tolerance geometrique
      cwipi_static_mesh, &
      cwipi_solver_cell_vertex, &
      1, &
      "Ensight Gold",&
      "text")
      ELSE 
      APPNAME = "code1"
      call cwipi_create_coupling_f("r2r", &
      cwipi_cpl_parallel_with_part,&
      APPNAME, &
      2,     & ! Dimension des entites geometriques
      tolerance, & ! Tolerance geometrique
      cwipi_static_mesh, &
      cwipi_solver_cell_vertex, &
      1, &
      "Ensight Gold",&
      "text")
      ENDIF 

      call cwipi_define_mesh_f("r2r", &
      coupling%send%numnod, &
      coupling%send%n_faces, &
      coords, &
      coupling%send%connectIndex, &
      coupling%send%connec)

      call cwipi_set_output_listing_f(6)
      call cwipi_dump_appli_properties_f()
      call cwipi_locate_f("r2r")
#endif 

        deallocate(coords)
        end subroutine coupling_init


        subroutine coupling_out(coupling,  nodes, t, quantity)

#ifdef WITH_CWIPI
          use cwipi
#endif
          use nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_), intent(inout) :: coupling !< coupling structure
          type(nodal_arrays_), intent(in) :: nodes !< Nodal arrays
          double precision, intent(in) :: t !< Time
          integer, intent(in) :: quantity !< Quantity to send
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
          if(coupling%send%quantity .NE. quantity) return
          ! Send the data to the other side
          if(.not.allocated(coupling%send%buffer)) then
            allocate(coupling%send%buffer(3*coupling%send%numnod))
          end if

          do i = 1, coupling%send%numnod
            if(coupling%send%quantity == coupling_COORDINATE) then
              coupling%send%buffer(3*i-2) = nodes%X(1,coupling%send%node_id(i)) 
              coupling%send%buffer(3*i-1) = nodes%X(2,coupling%send%node_id(i)) 
              coupling%send%buffer(3*i)   = nodes%X(3,coupling%send%node_id(i)) 
              fieldName = "COORDINATE"
            else if(coupling%send%quantity == coupling_VELOCITY) then
              coupling%send%buffer(3*i-2) = nodes%V(1,coupling%send%node_id(i))
              coupling%send%buffer(3*i-1) = nodes%V(2,coupling%send%node_id(i))
              coupling%send%buffer(3*i)   = nodes%V(3,coupling%send%node_id(i))
              fieldName = "VELOCITY"
            else if(coupling%send%quantity == coupling_ACCELERATION) then
              coupling%send%buffer(3*i-2) = nodes%a(1,coupling%send%node_id(i))
              coupling%send%buffer(3*i-1) = nodes%a(2,coupling%send%node_id(i))
              coupling%send%buffer(3*i)   = nodes%a(3,coupling%send%node_id(i))
              fieldName = "ACCELERATION"
            else if(coupling%send%quantity == coupling_FORCE) then
              coupling%send%buffer(3*i-2) = nodes%F(1,coupling%send%node_id(i))
              coupling%send%buffer(3*i-1) = nodes%F(2,coupling%send%node_id(i))
              coupling%send%buffer(3*i)   = nodes%F(3,coupling%send%node_id(i))
              fieldName = "FORCE"
            endif
          end do


#ifdef WITH_CWIPI
          call cwipi_update_location_f("r2r")
          call cwipi_locate_f("r2r")
          call cwipi_issend_f("r2r", &
                            "exchange", &
                            0, &
                            3, &
                            1, &
                            time, &
                            fieldName, &
                            coupling%send%buffer, &
                            coupling%send%rq)
           write(6,*) 'coupling_out: sended',size(coupling%send%buffer)
!           call cwipi_wait_issend_f("r2r",rq)
#endif
  
        end subroutine coupling_out

        subroutine coupling_out_wait(coupling)
#ifdef WITH_CWIPI
          use cwipi
#endif
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_), intent(inout) :: coupling !< coupling structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          write(6,*) 'coupling_out_wait: waiting for send to finish'
          call cwipi_wait_issend_f("r2r", coupling%send%rq)
        end subroutine coupling_out_wait
          
        subroutine coupling_in(coupling, nodes, t, quantity)
#ifdef WITH_CWIPI
          use cwipi
#endif
          use nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_), intent(inout) :: coupling !< coupling structure
          type(nodal_arrays_), intent(inout) :: nodes !< Nodal arrays
          double precision :: t !< Time
          integer :: quantity !< Quantity to receive
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
          write(6,*) 'coupling_in: coupling%recv%quantity:', coupling%recv%quantity, "quantity:", quantity

          if(coupling%recv%quantity .ne. quantity) then 
            return
          end if

          if(coupling%recv%quantity == NOTHING) then 
            return
          end if 

          if(.not.allocated(coupling%recv%buffer)) then
            allocate(coupling%recv%buffer(3*coupling%recv%numnod))
          end if
          time = t

          if(coupling%recv%quantity == coupling_COORDINATE) then
            fieldName = "COORDINATE"
          else if(coupling%recv%quantity == coupling_VELOCITY) then
            fieldName = "VELOCITY"
          else if(coupling%recv%quantity == coupling_ACCELERATION) then
            fieldName = "ACCELERATION"
          else if(coupling%recv%quantity == coupling_FORCE) then
            fieldName = "FORCE"
          endif

          ! Receive the data from the other side
#ifdef WITH_CWIPI
          write(6,*) 'coupling_in: cwipi_receive_f',size(coupling%recv%buffer)
          CALL cwipi_receive_f('r2r', &
                      'exchange', &
                      3, &
                      1, &
                      time, &
                      fieldName, &
                      coupling%recv%buffer, &
                      nNotLocatedPoints, &
                      status)
#endif
          ! compute the sum of the recv%buffer
          write(6,*) 'coupling_in: nNotLocatedPoints:', nNotLocatedPoints
          write(6,*) "summ of recv%buffer:", sum(coupling%recv%buffer)
          do i = 1, coupling%recv%numnod
            if(coupling%recv%quantity == coupling_COORDINATE) then
              nodes%X(1,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i-2)
              nodes%X(2,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i-1)
              nodes%X(3,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i)
            else if(coupling%recv%quantity == coupling_VELOCITY) then
              nodes%V(1,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i-2)
              nodes%V(2,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i-1)
              nodes%V(3,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i)
            else if(coupling%recv%quantity == coupling_ACCELERATION) then
              nodes%A(1,coupling%recv%node_id(i)) = nodes%A(1,coupling%recv%node_id(i)) + coupling%recv%buffer(3*i-2)
              nodes%A(2,coupling%recv%node_id(i)) = nodes%A(2,coupling%recv%node_id(i)) + coupling%recv%buffer(3*i-1)
              nodes%A(3,coupling%recv%node_id(i)) = nodes%A(3,coupling%recv%node_id(i)) + coupling%recv%buffer(3*i)
            else if(coupling%recv%quantity == coupling_FORCE) then
              nodes%F(1,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i-2)
              nodes%F(2,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i-1)
              nodes%F(3,coupling%recv%node_id(i)) = coupling%recv%buffer(3*i)
            endif
          end do
        end subroutine coupling_in



        subroutine coupling_free(coupling)
#ifdef WITH_CWIPI
          use cwipi
#endif
            implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(coupling_), intent(inout) :: coupling !< coupling structure

          if(allocated(coupling%send%buffer)) deallocate(coupling%send%buffer)
          if(allocated(coupling%send%connec)) deallocate(coupling%send%connec)
          if(allocated(coupling%send%node_id)) deallocate(coupling%send%node_id)
          if(allocated(coupling%send%connectIndex)) deallocate(coupling%send%connectIndex)
          if(allocated(coupling%recv%buffer)) deallocate(coupling%recv%buffer)
          if(allocated(coupling%recv%connec)) deallocate(coupling%recv%connec)
          if(allocated(coupling%recv%node_id)) deallocate(coupling%recv%node_id)
          if(allocated(coupling%recv%connectIndex)) deallocate(coupling%recv%connectIndex)
#ifdef WITH_CWIPI
          call cwipi_delete_coupling_f("r2r")
          call cwipi_finalize_f()
#endif
        end subroutine coupling_free
! ----------------------------------------------------------------------------------------------------------------------

      end module radioss_coupling_mod





