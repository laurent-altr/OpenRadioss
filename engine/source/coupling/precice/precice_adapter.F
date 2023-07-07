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

       module precice_adapter
         use precice

         implicit none

!=======================================================================================================================
!                                                  Included files
!=======================================================================================================================
#include "my_real.inc"
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
C list of avaiable data to exchange
           integer, parameter :: precice_displacements = 1
           integer, parameter :: precice_forces = 2
           integer, parameter :: precice_positions = 3
           character(len=precice_char_len), parameter :: precice_variable_names(3) =
     .  [ "Displacement                            ",
     .    "Force                                   ",
     .    "Position                                "]



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
              integer :: id               !< id of the precice data
              double precision, allocatable, dimension(:) :: values !< data array
            end type precice_data

            type precice_type
C              names
C             character(len=precice_char_len) :: participant_name !< name of the precice participant given by /PRECICE/PARTICIPANT_NAME
C             character(len=precice_char_len) :: config_file !< name of the precice config file
C             character(len=precice_char_len) :: mesh_name !< name of the precice mesh
C             character(len=precice_char_len) :: action_write_initial_data !< name of the precice action write initial data
C             character(len=precice_char_len) :: action_chekcpoint_read !< name of the precice action checkpoint read
C             character(len=precice_char_len) :: action_checkpoint_write !< name of the precice action checkpoint write
              character*50 :: participant_name !< name of the precice participant given by /PRECICE/PARTICIPANT_NAME
              character*50 :: config_file !< name of the precice config file
              character*50 :: mesh_name !< name of the precice mesh
              character*50 :: action_write_initial_data !< name of the precice action write initial data
              character*50 :: action_chekcpoint_read !< name of the precice action checkpoint read
              character*50 :: action_checkpoint_write !< name of the precice action checkpoint write

              ! data
              integer :: nb_coupling_nodes !< number of nodes in the coupling interface
              integer :: mesh_id !< id of the precice mesh
              integer :: grnod_id !< id of RADIOSS Group of nodes for the interface
              integer, dimension(:), allocatable :: vertex_ids !< ids of the nodes (preCICE numbering)
              integer, dimension(:), allocatable :: nodes_ids  !< ids of the nodes (Radioss numbering)
              double precision :: dt_limit !< time step limit
              type(precice_data) :: read_data !< data to be read from precice
              type(precice_data) :: write_data !< data to be written to precice
            end type precice_type

         interface 
           subroutine precice_read_cpl(input_filename, participant_name, config_file,
     .                                   mesh_name, write_name, read_name, grnod_id) 
     .     bind(c, name='cpp_precice_read_cpl')
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

           write(6,*) "data names=", precice%read_data%name, precice%write_data%name, 
     .                               precice%read_data%name_id, precice%write_data%name_id

           precice%action_checkpoint_write(1:precice_char_len)  ='                                                  '
           precice%action_chekcpoint_read(1:precice_char_len)   ='                                                  '
           precice%action_write_initial_data(1:precice_char_len)='                                                  '

#ifdef WITH_PRECICE 
           call precicef_action_write_initial_data(precice%action_write_initial_data, precice_char_len)
           call precicef_action_read_iter_checkp(precice%action_chekcpoint_read, precice_char_len)
           call precicef_action_write_iter_checkp(precice%action_checkpoint_write, precice_char_len)
#else
            write(6,*) "ERROR: precice is not available"
#endif

c          write(6,*) "action names=", precice%action_write_initial_data, precice%action_chekcpoint_read, precice%action_checkpoint_write
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

! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
           type(precice_type), intent(inout) :: precice !< precice adapter
           integer, intent(in) :: nb_nodes !< global number of nodes 
           integer, intent(in) :: mpi_rank !< my mpi rank (0-based)
           integer, intent(in) :: mpi_commsize !< number of mpi processes 
           my_real, intent(in) :: X(3,nb_nodes) !< coordinates
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
           call precicef_get_mesh_id(precice%mesh_name, precice%mesh_id, precice_char_len)

           do i = 1,nb_coupling_nodes
             k = precice%nodes_ids(i) 
             do j = 1,dimensions
               nodes((i - 1)*dimensions + j ) = X(j,k)
             enddo
           enddo
          
           if(allocated(precice%vertex_ids)) deallocate(precice%vertex_ids)
           allocate(precice%vertex_ids(nb_coupling_nodes))
#ifdef WITH_PRECICE
           call precicef_set_vertices(precice%mesh_id, nb_coupling_nodes, nodes, precice%vertex_ids)
#endif
           deallocate(nodes)
#ifdef WITH_PRECICE 
           call precicef_get_data_id(precice%read_data%name,precice%mesh_id,precice%read_data%id, precice_char_len)
           call precicef_get_data_id(precice%write_data%name,precice%mesh_id,precice%write_data%id, precice_char_len)
           write(6,"(A,A,I10,I10)") trim(precice%participant_name)," data ids= ",precice%read_data%id,precice%write_data%id
           write(6,"(A,A,A,A)") trim(precice%participant_name)," data names= ",trim(precice%read_data%name),trim(precice%write_data%name)

           call precicef_initialize(precice%dt_limit)
           call precicef_is_action_required(precice%action_write_initial_data, bool, precice_char_len)
           if(bool == 1) then
             call precicef_write_bvdata(precice%write_data%id, nb_coupling_nodes, precice%vertex_ids, precice%write_data%values)
             call precicef_mark_action_fulfilled(precice%action_write_initial_data, precice_char_len)
           endif           
           call precicef_initialize_data()
#endif

         end subroutine precice_initialize



!! \brief send the data to precice
         subroutine precice_write(precice,dt,global_values,nb_nodes,name_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
           type(precice_type), intent(inout) :: precice !< precice adapter
           integer, intent(in) :: nb_nodes !< number of nodes
           my_real, intent(in) :: global_values(3,nb_nodes) !< coordinates
           my_real, intent(in) :: dt !< time step
           integer, intent(in) :: name_id !< id of the data to be written
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
           integer :: i,k
           integer :: bool
           integer :: nb_coupling_nodes
           double precision :: dt_double
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
           write(6,*) "WRITE:",name_id, precice%write_data%name_id, precice%write_data%name

           if(name_id /= precice%write_data%name_id) return
           nb_coupling_nodes = precice%nb_coupling_nodes
           dt_double = dt
           bool = 0
#ifdef WITH_PRECICE
           call precicef_is_write_data_required(dt_double, bool)
#endif
           if (bool.eq.1) then
              write(6,*) trim(precice%participant_name)," write required ",precice%write_data%name,precice%write_data%id,
     .      precice%write_data%values(1),precice%write_data%values(2),precice%write_data%values(3)

              do i = 1,nb_coupling_nodes
                k = precice%nodes_ids(i)
                precice%write_data%values((i - 1)*3 + 1 ) = global_values(1,k)
                precice%write_data%values((i - 1)*3 + 2 ) = global_values(2,k)
                precice%write_data%values((i - 1)*3 + 3 ) = global_values(3,k)
c               write(6,*)  uid(k),precice%write_data%values((i - 1)*3 + 1 )
c               write(6,*)  uid(k),precice%write_data%values((i - 1)*3 + 2 )
c               write(6,*)  uid(k),precice%write_data%values((i - 1)*3 + 3 )
              enddo
#ifdef WITH_PRECICE
             call precicef_write_bvdata(precice%write_data%id, nb_coupling_nodes, 
     .                                  precice%vertex_ids, precice%write_data%values)
#endif
           else
             write(6,*) trim(precice%participant_name)," write NOT required "
           endif
         end subroutine

         subroutine precice_read(precice,dt,global_values,nb_nodes,mode,name_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
           type(precice_type), intent(inout) :: precice !< precice adapter
           integer, intent(in) :: nb_nodes !< number of nodes
           integer, intent(in) :: mode !< mode = 1, replace the values, mode = 2, add the values
           double precision, intent(in) :: dt !< time step
           my_real, intent(inout) :: global_values(3,nb_nodes) !< coordinates
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
           integer :: bool
           integer :: nb_coupling_nodes
           double precision :: dt_double
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
           dt_double = dt
           nb_coupling_nodes = precice%nb_coupling_nodes
c          write(6,*) "READ:",name_id, precice%read_data%name_id
           if(name_id /= precice%read_data%name_id) return
#ifdef WITH_PRECICE
           call precicef_is_read_data_available(bool)
           if (bool.eq.1) then
             write(6,*) trim(precice%participant_name)," read avail ",precice%read_data%name,precice%read_data%id
             call precicef_read_bvdata(precice%read_data%id, nb_coupling_nodes, 
     .                                  precice%vertex_ids, precice%read_data%values)

           else
              write(6,*) trim(precice%participant_name)," read NOT avail ",precice%read_data%name,precice%read_data%id
           endif
#endif
           if(bool .eq. 0) return
           if( mode == replace ) then
             do i = 1,nb_coupling_nodes
               k = precice%nodes_ids(i)
               global_values(1,k) = precice%read_data%values((i - 1)*3 + 1 )
               global_values(2,k) = precice%read_data%values((i - 1)*3 + 2 )
               global_values(3,k) = precice%read_data%values((i - 1)*3 + 3 )
c              write(6,*) uid(k),precice%read_data%values((i - 1)*3 + 1 )
c              write(6,*) uid(k),precice%read_data%values((i - 1)*3 + 2 )
c              write(6,*) uid(k),precice%read_data%values((i - 1)*3 + 3 )
             enddo
           elseif( mode == add ) then
             do i = 1,nb_coupling_nodes
               k = precice%nodes_ids(i)
               global_values(1,k) = global_values(1,k) + precice%read_data%values((i - 1)*3 + 1 )
               global_values(2,k) = global_values(2,k) + precice%read_data%values((i - 1)*3 + 2 )
               global_values(3,k) = global_values(3,k) + precice%read_data%values((i - 1)*3 + 3 )
c              write(6,*) uid(k),precice%read_data%values((i - 1)*3 + 1 )
c              write(6,*) uid(k),precice%read_data%values((i - 1)*3 + 2 )
c              write(6,*) uid(k),precice%read_data%values((i - 1)*3 + 3 )
             enddo
           endif
         end subroutine

!! \brief finalize the precice adapter
!! \details deallocate the arrays and finalize precice
         subroutine precice_finalize(precice)
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
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
           type(precice_type), intent(inout) :: precice !< precice adapter
           my_real, intent(inout) :: dt !< time step
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
           double precision :: dt_double
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Body
! ----------------------------------------------------------------------------------------------------------------------
           dt_double = min(dt, precice%dt_limit)
#ifdef WITH_PRECICE
           call precicef_advance(dt_double)
#endif
           write(6,*) " advance ",dt,dt_double,precice%dt_limit
           precice%dt_limit = dt_double
           dt_double = min(dt_double, dble(dt))
           dt = dt_double
         end subroutine

! \brief return true if the simulation is ongoing
         subroutine precice_ongoing(ongoing)
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
      end module precice_adapter