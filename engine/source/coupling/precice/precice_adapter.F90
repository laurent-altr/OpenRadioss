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
module precice_adapter_mod
        implicit none

!=======================================================================================================================
!                                                  Included files
!=======================================================================================================================
!=======================================================================================================================
!                                                  Module parameters
!=======================================================================================================================
#ifdef WITH_PRECICE
        logical, parameter :: precice_available = .true.
#else
        logical, parameter :: precice_available = .false.
#endif
        integer, parameter :: precice_char_len = 50
        integer, parameter :: dimensions = 3
! list of avaiable data to exchange
        integer, parameter :: precice_displacements = 1
        integer, parameter :: precice_forces = 2
        integer, parameter :: precice_positions = 3
        character(len=precice_char_len), parameter :: precice_variable_names(3) =&
        &[ "Displacement                            ",&
        &"Force                                   ",&
        &"Position                                "]



!=======================================================================================================================
!                                                  Type definitions
!===================================i====================================================================================
        ! /PRECICE/PARTICIPANT_NAME/name
        ! /PRECICE/CONFIG_FILE/name
        ! /PRECICE/MESH_NAME/name
        ! available data are
        ! /PRECICE/[READ|WRITE]/DISPLACEMENTS
        ! /PRECICE/[READ|WRITE]/FORCES
        ! Coupling mesh is defined by
        ! /PRECICE/INTERFACE/GRNOD/id
        type :: precice_data
          character(len=precice_char_len) :: name  !< name of the precice data
          integer :: name_id          !< id of the precice data defined by parameters
          ! NOTE: 'id' field removed - no longer needed in v3.x (uses names directly)
          double precision, allocatable, dimension(:) :: values !< data array
        end type precice_data

        type precice_type
          character*50 :: participant_name !< name of the precice participant given by /PRECICE/PARTICIPANT_NAME
          character*50 :: config_file !< name of the precice config file
          character*50 :: mesh_name !< name of the precice mesh
          ! NOTE: Action strings removed - v3.x uses direct function calls

          ! data
          integer :: nb_coupling_nodes !< number of nodes in the coupling interface
          ! NOTE: mesh_id removed - v3.x uses mesh names directly
          integer :: grnod_id !< id of RADIOSS Group of nodes for the interface
          integer, dimension(:), allocatable :: vertex_ids !< ids of the nodes (preCICE numbering)
          integer, dimension(:), allocatable :: nodes_ids  !< ids of the nodes (Radioss numbering)
          double precision :: dt_limit !< time step limit
          type(precice_data) :: read_data !< data to be read from precice
          type(precice_data) :: write_data !< data to be written to precice
        end type precice_type

        interface
          subroutine precice_read_cpl(input_filename, participant_name, config_file,&
          &mesh_name, write_name, read_name, grnod_id)&
          &bind(c, name='cpp_precice_read_cpl')
            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: input_filename
            character(kind=c_char), dimension(*) :: participant_name
            character(kind=c_char), dimension(*) :: config_file
            character(kind=c_char), dimension(*) :: mesh_name
            character(kind=c_char), dimension(*) :: write_name
            character(kind=c_char), dimension(*) :: read_name
            integer(kind=c_int)    :: grnod_id
          end subroutine precice_read_cpl
        end interface

      contains

!=======================================================================================================================
!                                                  Subroutines
!=======================================================================================================================
!! \brief remove C_NULL_CHAR from a string
        subroutine remove_null_char(str)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Modules
! ----------------------------------------------------------------------------------------------------------------------
          use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=50), intent(inout) :: str !< string to be found, will be modified
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          do i = 1, len(str)
            if (str(i:i) == C_NULL_CHAR) then
              str(i:i) = ' '
              exit
            end if
          end do
        end subroutine remove_null_char

!! \brief find the position in to the parameter names
        function find_variable_name_id(str) result(id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Modules
! ----------------------------------------------------------------------------------------------------------------------
          use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=50), intent(inout) :: str !< string to be found, will be modified
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: id, i
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          id = 0
          ! replace C null character by a whitespace
          call remove_null_char(str)
          do i = 1, size(precice_variable_names)
            if (trim(str) == trim(precice_variable_names(i))) then
              id = i
              exit
            end if
          end do
        end function

!! \brief This subroutine reads the precice input file *.cpl
        subroutine precice_read_file(precice, input_filename)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precice_mod
          use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(precice_type), intent(inout) :: precice !< precice adapter
          character(len=precice_char_len) :: input_filename !< name of the precice input file
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(len=precice_char_len) :: participant_name !< name of the precice participant given by /PRECICE/PARTICIPANT_NAME
          character(len=precice_char_len) :: config_file !< name of the precice config file
          character(len=precice_char_len) :: mesh_name !< name of the precice mesh
          character(len=precice_char_len) :: read_data_name !< name of the precice data to be read
          character(len=precice_char_len) :: write_data_name !< name of the precice data to be written
          integer :: grnod_id !< id of GRNOD of the interface
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          precice%mesh_name = repeat(" ", precice_char_len)
          precice%read_data%name = repeat(" ", precice_char_len)
          precice%write_data%name = repeat(" ", precice_char_len)
          precice%participant_name = repeat(" ", precice_char_len)
          precice%config_file = repeat(" ", precice_char_len)

          mesh_name = repeat(" ", precice_char_len)
          read_data_name = repeat(" ", precice_char_len)
          write_data_name = repeat(" ", precice_char_len)
          participant_name = repeat(" ", precice_char_len)
          config_file = repeat(" ", precice_char_len)

          call precice_read_cpl(input_filename, participant_name, config_file, mesh_name, write_data_name, read_data_name, grnod_id)

          call remove_null_char(participant_name)
          call remove_null_char(config_file)
          call remove_null_char(mesh_name)
          call remove_null_char(read_data_name)
          call remove_null_char(write_data_name)
          precice%grnod_id = grnod_id
          precice%config_file(1:len_trim(config_file)) = config_file
          precice%participant_name(1:len_trim(participant_name)) = participant_name
          precice%mesh_name(1:len_trim(mesh_name)) = mesh_name
          precice%read_data%name(1:len_trim(read_data_name)) = read_data_name
          precice%write_data%name(1:len_trim(write_data_name)) = write_data_name
          precice%read_data%name_id = find_variable_name_id(precice%read_data%name)
          precice%write_data%name_id = find_variable_name_id(precice%write_data%name)

          write(6,*) "data names=", precice%read_data%name, precice%write_data%name,&
          &precice%read_data%name_id, precice%write_data%name_id

          ! NOTE: Action strings no longer needed in v3.x - removed action setup
#ifndef WITH_PRECICE
          write(6,*) "ERROR: precice is not available"
#endif

        end subroutine precice_read_file

!! \brief This subroutine set the ids of the nodes in the coupling interface.
!!    it also allocates the memory for the data to be exchanged with precice.
        subroutine precice_set_nodes(precice, igrnod, ngrnod)
          use GROUPDEF_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ngrnod !< number of GRNOD
          type(precice_type), intent(inout) :: precice !< precice adapter
          type(GROUP_), intent(in) :: igrnod(ngrnod) !< GRNOD of the interface
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nb_coupling_nodes !< number of coupling nodes
          integer :: group_id
          integer :: i,j
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          j = 0
          do i = 1,NGRNOD
            if(igrnod(i)%id == precice%grnod_id) then
              j = i
              exit
            endif
          enddo
          if(j == 0) then
            write(6,*) "ERROR: precice_set_nodes: grnod_id not found"
            stop
          endif
          nb_coupling_nodes = igrnod(j)%nentity
          precice%nb_coupling_nodes = nb_coupling_nodes
          if(allocated(precice%nodes_ids)) deallocate(precice%nodes_ids)
          allocate(precice%nodes_ids(nb_coupling_nodes))
          precice%nodes_ids(1:nb_coupling_nodes) = igrnod(j)%entity(1:nb_coupling_nodes)

          if(allocated(precice%read_data%values)) deallocate(precice%read_data%values)
          allocate(precice%read_data%values(nb_coupling_nodes*dimensions))
          precice%read_data%values = 0.0D0
          if(allocated(precice%write_data%values)) deallocate(precice%write_data%values)
          allocate(precice%write_data%values(nb_coupling_nodes*dimensions))
          precice%write_data%values = 0.0D0

        end subroutine precice_set_nodes

!! \brief This subroutine initializes the precice adapter
        subroutine precice_initialize(precice,X,nb_nodes,mpi_rank,mpi_commsize)
          use precice_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(precice_type), intent(inout) :: precice !< precice adapter
          integer, intent(in) :: nb_nodes !< global number of nodes
          integer, intent(in) :: mpi_rank !< my mpi rank (0-based)
          integer, intent(in) :: mpi_commsize !< number of mpi processes
          real(kind=WP), intent(in) :: X(3,nb_nodes) !< coordinates
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k
          integer :: nb_coupling_nodes
          double precision, dimension(:), allocatable :: nodes
          integer :: bool
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
          nb_coupling_nodes = precice%nb_coupling_nodes
          write(6,*) "participant=",precice%participant_name
          write(6,*) "config_file=",precice%config_file
          write(6,*) "mpi_rank=",mpi_rank,mpi_commsize

#ifdef WITH_PRECICE
          call precicef_create(precice%participant_name,precice%config_file,mpi_rank,mpi_commsize,50,50)
#else
          write(6,*) "ERROR: precice is not available"
          stop
#endif

          allocate(nodes(nb_coupling_nodes*dimensions))
          
          ! NOTE: No longer need to get mesh_id - use mesh name directly
          do i = 1,nb_coupling_nodes
            k = precice%nodes_ids(i)
            do j = 1,dimensions
              nodes((i - 1)*dimensions + j ) = X(j,k)
            enddo
          enddo

          if(allocated(precice%vertex_ids)) deallocate(precice%vertex_ids)
          allocate(precice%vertex_ids(nb_coupling_nodes))
#ifdef WITH_PRECICE
          ! v3.x: Use mesh name instead of mesh ID
          call precicef_set_vertices(precice%mesh_name, nb_coupling_nodes, nodes, precice%vertex_ids, precice_char_len)
#endif
          deallocate(nodes)

          ! NOTE: No longer need to get data IDs - use data names directly in read/write calls
          write(6,"(A,A)") trim(precice%participant_name)," data names= "//trim(precice%read_data%name)//" "//trim(precice%write_data%name)

#ifdef WITH_PRECICE
          ! v3.x: Check if initial data is required and write it BEFORE initialize()
          call precicef_requires_initial_data(bool)
          if(bool == 1) then
            write(6,*) trim(precice%participant_name)," writing initial data"
            call precicef_write_data(precice%mesh_name, precice%write_data%name, precice%vertex_ids, &
                 & nb_coupling_nodes, precice%write_data%values, precice_char_len, precice_char_len)
          endif

          ! v3.x: initialize() no longer returns dt_limit and handles data initialization internally
          call precicef_initialize()
          
          ! v3.x: Get max time step size separately
          call precicef_get_max_time_step_size(precice%dt_limit)
#endif

        end subroutine precice_initialize

!! \brief send the data to precice
        subroutine precice_write(precice,dt,global_values,nb_nodes,name_id)
          use precision_mod, only : WP
          use precice_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(precice_type), intent(inout) :: precice !< precice adapter
          integer, intent(in) :: nb_nodes !< number of nodes
          real(kind=WP), intent(in) :: global_values(3,nb_nodes) !< coordinates
          real(kind=WP), intent(in) :: dt !< time step
          integer, intent(in) :: name_id !< id of the data to be written
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,k
          integer :: nb_coupling_nodes
          double precision :: dt_double
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
          write(6,*) "WRITE:",name_id, precice%write_data%name_id, precice%write_data%name

          if(name_id /= precice%write_data%name_id) return
          nb_coupling_nodes = precice%nb_coupling_nodes
          dt_double = dt

          ! v3.x: Writing is always possible (generates samples for time interpolation)
          ! No need to check precicef_is_write_data_required
          write(6,*) trim(precice%participant_name)," writing data ",precice%write_data%name,&
          &precice%write_data%values(1),precice%write_data%values(2),precice%write_data%values(3)

          do i = 1,nb_coupling_nodes
            k = precice%nodes_ids(i)
            precice%write_data%values((i - 1)*3 + 1 ) = global_values(1,k)
            precice%write_data%values((i - 1)*3 + 2 ) = global_values(2,k)
            precice%write_data%values((i - 1)*3 + 3 ) = global_values(3,k)
          enddo
#ifdef WITH_PRECICE
          ! v3.x: Use mesh name and data name instead of data ID
          call precicef_write_data(precice%mesh_name, precice%write_data%name, precice%vertex_ids, &
               & nb_coupling_nodes, precice%write_data%values, precice_char_len, precice_char_len)
#endif

        end subroutine

        subroutine precice_read(precice,dt,global_values,nb_nodes,mode,name_id)
          use precice_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(precice_type), intent(inout) :: precice !< precice adapter
          integer, intent(in) :: nb_nodes !< number of nodes
          integer, intent(in) :: mode !< mode = 1, replace the values, mode = 2, add the values
          double precision, intent(in) :: dt !< time step
          real(kind=WP), intent(inout) :: global_values(3,nb_nodes) !< coordinates
          integer, intent(in) :: name_id !< id of the data to be read
! ----------------------------------------------------------------------------------------------------------------------
!                                                  Local parameters
! ----------------------------------------------------------------------------------------------------------------------
          integer, parameter :: replace = 1
          integer, parameter :: add = 2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,k
          integer :: nb_coupling_nodes
          double precision :: dt_double
          double precision :: relative_read_time
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
          dt_double = dt
          nb_coupling_nodes = precice%nb_coupling_nodes
          if(name_id /= precice%read_data%name_id) return

          ! v3.x: Reading is always possible with time interpolation
          ! Use dt as relative read time (reads at end of current time step)
          relative_read_time = dt_double
          
          write(6,*) trim(precice%participant_name)," reading data ",precice%read_data%name

#ifdef WITH_PRECICE
          ! v3.x: Use mesh name, data name, and relative read time
          call precicef_read_data(precice%mesh_name, precice%read_data%name, precice%vertex_ids, &
               & nb_coupling_nodes, relative_read_time, precice%read_data%values, &
               & precice_char_len, precice_char_len)
#endif

          if( mode == replace ) then
            do i = 1,nb_coupling_nodes
              k = precice%nodes_ids(i)
              global_values(1,k) = precice%read_data%values((i - 1)*3 + 1 )
              global_values(2,k) = precice%read_data%values((i - 1)*3 + 2 )
              global_values(3,k) = precice%read_data%values((i - 1)*3 + 3 )
            enddo
          elseif( mode == add ) then
            do i = 1,nb_coupling_nodes
              k = precice%nodes_ids(i)
              global_values(1,k) = global_values(1,k) + precice%read_data%values((i - 1)*3 + 1 )
              global_values(2,k) = global_values(2,k) + precice%read_data%values((i - 1)*3 + 2 )
              global_values(3,k) = global_values(3,k) + precice%read_data%values((i - 1)*3 + 3 )
            enddo
          endif
        end subroutine

!! \brief finalize the precice adapter
!! \details deallocate the arrays and finalize precice
        subroutine precice_finalize(precice)
          use precice_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(precice_type), intent(inout) :: precice !< precice adapter
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
#ifdef WITH_PRECICE
          call precicef_finalize()
#endif
          if(allocated(precice%nodes_ids)) deallocate(precice%nodes_ids)
          if(allocated(precice%vertex_ids)) deallocate(precice%vertex_ids)
          if(allocated(precice%read_data%values)) deallocate(precice%read_data%values)
          if(allocated(precice%write_data%values)) deallocate(precice%write_data%values)

        end subroutine

        subroutine precice_advance(precice,dt)
          use precision_mod, only : WP
          use precice_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(precice_type), intent(inout) :: precice !< precice adapter
          real(kind=WP), intent(inout) :: dt !< time step
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision :: dt_double
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
          dt_double = min(dt, precice%dt_limit)
#ifdef WITH_PRECICE
          ! v3.x: advance() no longer returns dt_limit
          call precicef_advance(dt_double)
          
          ! v3.x: Get new max time step size after advance
          call precicef_get_max_time_step_size(precice%dt_limit)
#endif
          write(6,*) " advance ",dt,dt_double,precice%dt_limit
          dt_double = min(dt_double, dble(dt))
          dt = dt_double
        end subroutine

! \brief return true if the simulation is ongoing
        subroutine precice_ongoing(ongoing)
          use precice_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, intent(inout) :: ongoing !< true if the simulation is ongoing
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: bool
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
          ongoing = .false.
#ifdef WITH_PRECICE
          call precicef_is_coupling_ongoing(bool)
          if (bool.eq.1) then
            ongoing = .true.
          endif
#endif
        end subroutine

        ! v3.x: Add checkpoint handling functions for implicit coupling
        subroutine precice_requires_writing_checkpoint(required)
          use precice_mod
          logical, intent(out) :: required
          integer :: bool
          
          required = .false.
#ifdef WITH_PRECICE
          call precicef_requires_writing_checkpoint(bool)
          if (bool == 1) required = .true.
#endif
        end subroutine

        subroutine precice_requires_reading_checkpoint(required)
          use precice_mod
          logical, intent(out) :: required
          integer :: bool
          
          required = .false.
#ifdef WITH_PRECICE
          call precicef_requires_reading_checkpoint(bool)
          if (bool == 1) required = .true.
#endif
        end subroutine

      end module precice_adapter_mod
