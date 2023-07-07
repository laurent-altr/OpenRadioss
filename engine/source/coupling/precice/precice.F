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
       module precice
         use, intrinsic :: iso_c_binding
         implicit none

#ifdef WITH_PRECICE
         interface

           subroutine precicef_create(participant_name, config_file_name,
     .                         mpi_rank, mpi_commsize,
     .                         length_accessor_name, length_config_file_name)
     .     bind(c, name='precicef_create_')

             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: participant_name
             character(kind=c_char), dimension(*) :: config_file_name
             integer(kind=c_int) :: mpi_rank
             integer(kind=c_int) :: mpi_commsize
             integer(kind=c_int), value :: length_accessor_name
             integer(kind=c_int), value :: length_config_file_name
           end subroutine precicef_create

           subroutine precicef_initialize(dt_limit)
     .     bind(c, name='precicef_initialize_')

             use, intrinsic :: iso_c_binding
             real(kind=c_double) :: dt_limit
           end subroutine precicef_initialize

           subroutine precicef_initialize_data()
     .     bind(c, name='precicef_initialize_data_')

             use, intrinsic :: iso_c_binding
           end subroutine precicef_initialize_data

           subroutine precicef_advance(dt_limit)
     .     bind(c, name='precicef_advance_')

             use, intrinsic :: iso_c_binding
             real(kind=c_double) :: dt_limit
           end subroutine precicef_advance

           subroutine precicef_finalize()
     .     bind(c, name='precicef_finalize_')

             use, intrinsic :: iso_c_binding
           end subroutine precicef_finalize

           subroutine precicef_get_dims(dimensions)
     .     bind(c, name='precicef_get_dims_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: dimensions
           end subroutine precicef_get_dims

           subroutine precicef_is_coupling_ongoing(ongoing)
     .     bind(c, name='precicef_is_coupling_ongoing_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: ongoing
           end subroutine precicef_is_coupling_ongoing

           subroutine precicef_is_read_data_available(is_available)
     .     bind(c, name='precicef_is_read_data_available_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: is_available
           end subroutine precicef_is_read_data_available

           subroutine precicef_is_write_data_required(dt,
     .                                        is_required)
     .     bind(c, name='precicef_is_write_data_required_')

             use, intrinsic :: iso_c_binding
             real(kind=c_double) :: dt
             integer(kind=c_int) :: is_required
           end subroutine precicef_is_write_data_required

           subroutine precicef_is_time_window_complete(is_complete)
     .     bind(c, name='precicef_is_time_window_complete_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: is_complete
           end subroutine precicef_is_time_window_complete

           subroutine precicef_has_to_evaluate_surrogate_model(has_to_evaluate)
     .     bind(c, name='precicef_has_to_evaluate_surrogate_model_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: has_to_evaluate
           end subroutine precicef_has_to_evaluate_surrogate_model

           subroutine precicef_has_to_evaluate_fine_model(has_to_evaluate)
     .     bind(c, name='precicef_has_to_evaluate_fine_model_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: has_to_evaluate
           end subroutine precicef_has_to_evaluate_fine_model

           subroutine precicef_is_action_required(action, is_required, length_action)
     .     bind(c, name='precicef_is_action_required_')

             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: action
             integer(kind=c_int)                  :: is_required
             integer(kind=c_int), value           :: length_action
           end subroutine precicef_is_action_required

           subroutine precicef_mark_action_fulfilled(action, length_action)
     .     bind(c, name='precicef_mark_action_fulfilled_')

             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: action
             integer(kind=c_int), value           :: length_action
           end subroutine precicef_mark_action_fulfilled

           subroutine precicef_has_mesh(mesh_name, has_mesh, length_mesh_name)
     .     bind(c, name='precicef_has_mesh_')

             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: mesh_name
             integer(kind=c_int)                  :: has_mesh
             integer(kind=c_int), value           :: length_mesh_name
           end subroutine precicef_has_mesh

           subroutine precicef_get_mesh_id(mesh_name, mesh_id, length_mesh_name)
     .     bind(c, name='precicef_get_mesh_id_')

             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: mesh_name
             integer(kind=c_int)                  :: mesh_id
             integer(kind=c_int), value           :: length_mesh_name
           end subroutine precicef_get_mesh_id

           subroutine precicef_set_vertex(mesh_id, position, vertex_id)
     .     bind(c, name='precicef_set_vertex_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             real(kind=c_double) :: position(3)
             integer(kind=c_int) :: vertex_id
           end subroutine precicef_set_vertex

           subroutine precicef_get_mesh_vertex_size(mesh_id, mesh_size)
     .     bind(c, name='precicef_get_mesh_vertex_size_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: mesh_size
           end subroutine precicef_get_mesh_vertex_size

           subroutine precicef_set_vertices(mesh_id, mesh_size, positions, position_ids)
     .     bind(c, name='precicef_set_vertices_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: mesh_size
             real(kind=c_double) :: positions(*)
             integer(kind=c_int) :: position_ids(*)
           end subroutine precicef_set_vertices

           subroutine precicef_get_vertices(mesh_id, size, ids, positions )
     .     bind(c, name='precicef_get_vertices_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: size
             integer(kind=c_int) :: ids(*)
             real(kind=c_double) :: positions(*)
           end subroutine precicef_get_vertices

           subroutine precicef_get_vertex_ids_from_positions(mesh_id, size, positions, ids )
     .     bind(c, name='precicef_get_vertices_from_positions_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: size
             real(kind=c_double) :: positions(*)
             integer(kind=c_int) :: ids(*)
           end subroutine precicef_get_vertex_ids_from_positions

           subroutine precicef_set_edge(mesh_id, first_vertex_id, second_vertex_id,
     .                           edge_id)
     .     bind(c, name='precicef_set_edge_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: first_vertex_id
             integer(kind=c_int) :: second_vertex_id
             integer(kind=c_int) :: edge_id
           end subroutine precicef_set_edge

           subroutine precicef_set_triangle(mesh_id, first_edge_id, second_edge_id,
     .                               third_edge_id)
     .     bind(c, name='precicef_set_triangle_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: first_edge_id
             integer(kind=c_int) :: second_edge_id
             integer(kind=c_int) :: third_edge_id
           end subroutine precicef_set_triangle

           subroutine precicef_set_tetrahedron(mesh_id, first_vertex_id, second_vertex_id,
     .                                  third_vertex_id, fourth_vertex_id)
     .     bind(c, name='precicef_set_tetrahedron')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: first_vertex_id
             integer(kind=c_int) :: second_vertex_id
             integer(kind=c_int) :: third_vertex_id
             integer(kind=c_int) :: fourth_vertex_id
           end subroutine precicef_set_tetrahedron

           subroutine precicef_set_triangle_we(mesh_id, first_vertex_id, second_vertex_id,
     .                               third_vertex_id)
     .     bind(c, name='precicef_set_triangle_we_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: first_vertex_id
             integer(kind=c_int) :: second_vertex_id
             integer(kind=c_int) :: third_vertex_id
           end subroutine precicef_set_triangle_we

           subroutine precicef_set_quad(mesh_id, first_edge_id, second_edge_id,
     .                               third_edge_id, fourth_edge_id )
     .     bind(c, name='precicef_set_quad_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: first_edge_id
             integer(kind=c_int) :: second_edge_id
             integer(kind=c_int) :: third_edge_id
             integer(kind=c_int) :: fourth_edge_id
           end subroutine precicef_set_quad

           subroutine precicef_set_quad_we(mesh_id, first_vertex_id, second_vertex_id,
     .                               third_vertex_id, fourth_vertex_id)
     .     bind(c, name='precicef_set_quad_we_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: first_vertex_id
             integer(kind=c_int) :: second_vertex_id
             integer(kind=c_int) :: third_vertex_id
             integer(kind=c_int) :: fourth_vertex_id
           end subroutine precicef_set_quad_we

           subroutine precicef_has_data(data_name, mesh_id, hasData, length_data_name)
     .     bind(c, name='precicef_has_data_')

             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: data_name
             integer(kind=c_int)                  :: mesh_id
             integer(kind=c_int)                  :: hasData
             integer(kind=c_int), value           :: length_data_name
           end subroutine precicef_has_data

           subroutine precicef_get_data_id(data_name, mesh_id, data_id, length_data_name )
     .     bind(c, name='precicef_get_data_id_')

             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: data_name
             integer(kind=c_int)                  :: mesh_id
             integer(kind=c_int)                  :: data_id
             integer(kind=c_int), value           :: length_data_name
           end subroutine precicef_get_data_id

           subroutine precicef_is_mesh_connectivity_required(mesh_id, required)
     .     bind(c, name='precicef_is_mesh_connectivity_required_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
             integer(kind=c_int) :: required
           end subroutine precicef_is_mesh_connectivity_required

           subroutine precicef_map_read_data_to(mesh_id)
     .     bind(c, name='precicef_map_read_data_to_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
           end subroutine precicef_map_read_data_to

           subroutine precicef_map_write_data_from(mesh_id)
     .     bind(c, name='precicef_map_write_data_from_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: mesh_id
           end subroutine precicef_map_write_data_from

           subroutine precicef_write_sdata( data_id, value_index, data_value)
     .     bind(c, name='precicef_write_sdata_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: value_index
             real(kind=c_double) :: data_value
           end subroutine precicef_write_sdata

           subroutine precicef_write_bsdata( data_id, block_size, value_indices, values)
     .     bind(c, name='precicef_write_bsdata_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: block_size
             integer(kind=c_int) :: value_indices(*)
             real(kind=c_double) :: values(*)
           end subroutine precicef_write_bsdata

           subroutine precicef_write_vdata( data_id, value_index, data_value)
     .     bind(c, name='precicef_write_vdata_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: value_index
             real(kind=c_double), dimension(*) :: data_value
           end subroutine precicef_write_vdata

           subroutine precicef_write_bvdata( data_id, block_size, value_indices, values)
     .     bind(c, name='precicef_write_bvdata_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: block_size
             integer(kind=c_int) :: value_indices(*)
             real(kind=c_double) :: values(*)
           end subroutine precicef_write_bvdata

           subroutine precicef_read_sdata( data_id, value_index, data_value)
     .     bind(c, name='precicef_read_sdata_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: value_index
             real(kind=c_double) :: data_value
           end subroutine precicef_read_sdata

           subroutine precicef_read_bsdata( data_id, block_size, value_indices, values)
     .     bind(c, name='precicef_read_bsdata_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: block_size
             integer(kind=c_int) :: value_indices(*)
             real(kind=c_double) :: values(*)
           end subroutine precicef_read_bsdata

           subroutine precicef_read_vdata( data_id, value_index, data_value)
     .     bind(c, name='precicef_read_vdata_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: value_index
             real(kind=c_double) :: data_value(*)
           end subroutine precicef_read_vdata

           subroutine precicef_read_bvdata( data_id, block_size, value_indices, values)
     .     bind(c, name='precicef_read_bvdata_')

             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: block_size
             integer(kind=c_int) :: value_indices(*)
             real(kind=c_double) :: values(*)
           end subroutine precicef_read_bvdata

           subroutine precicef_action_write_iter_checkp(name_action, length_name_action)
     .         bind(c, name="precicef_action_write_iter_checkp_")
             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: name_action
             integer(kind=c_int), value :: length_name_action
           end subroutine precicef_action_write_iter_checkp

           subroutine precicef_action_write_initial_data(name_action, length_name_action)
     .         bind(c, name="precicef_action_write_initial_data_")
             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: name_action
             integer(kind=c_int), value :: length_name_action
           end subroutine precicef_action_write_initial_data

           subroutine precicef_action_read_iter_checkp(name_action, length_name_action)
     .         bind(c, name="precicef_action_read_iter_checkp_")
             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: name_action
             integer(kind=c_int), value :: length_name_action
           end subroutine precicef_action_read_iter_checkp

           subroutine precicef_get_version_information(version_info, length_version_info)
     .         bind(c, name="precicef_get_version_information_")
             use, intrinsic :: iso_c_binding
             character(kind=c_char), dimension(*) :: version_info
             integer(kind=c_int), value :: length_version_info
           end subroutine precicef_get_version_information

           subroutine precicef_is_gradient_data_required(data_id, is_required)
     .         bind(c, name="precicef_is_gradient_data_required_")
             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: is_required
           end subroutine precicef_is_gradient_data_required

           subroutine precicef_write_sgradient_data(data_id, value_index, data_value)
     .         bind(c, name="precicef_write_sgradient_data_")
             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: value_index
             real(kind=c_double) :: data_value(*)
           end subroutine precicef_write_sgradient_data

           subroutine precicef_write_bsgradient_data(data_id,size,value_indices,gradient_values)
     .         bind(c, name="precicef_write_bsgradient_data_")
             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: size
             integer(kind=c_int) :: value_indices(*)
             real(kind=c_double) :: gradient_values(*)
           end subroutine precicef_write_bsgradient_data

           subroutine precicef_write_vgradient_data(data_id, value_index, data_value)
     .         bind(c, name="precicef_write_vgradient_data_")
             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: value_index
             real(kind=c_double) :: data_value(*)
           end subroutine precicef_write_vgradient_data

           subroutine precicef_write_bvgradient_data(data_id, block_size, value_indices, gradient_values)
     .         bind(c, name="precicef_write_bvgradient_data_")
             use, intrinsic :: iso_c_binding
             integer(kind=c_int) :: data_id
             integer(kind=c_int) :: block_size
             integer(kind=c_int) :: value_indices(*)
             real(kind=c_double) :: gradient_values(*)
           end subroutine precicef_write_bvgradient_data
         end interface
#endif
       end module precice
