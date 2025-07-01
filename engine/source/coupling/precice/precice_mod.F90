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
module precice_mod
        use, intrinsic :: iso_c_binding
        implicit none

#ifdef WITH_PRECICE
        interface

          ! Constructor - now creates Participant instead of SolverInterface
          subroutine precicef_create(participant_name, config_file_name,&
          &mpi_rank, mpi_commsize,&
          &length_accessor_name, length_config_file_name)&
          &bind(c, name='precicef_create_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: participant_name
            character(kind=c_char), dimension(*) :: config_file_name
            integer(kind=c_int) :: mpi_rank
            integer(kind=c_int) :: mpi_commsize
            integer(kind=c_int), value :: length_accessor_name
            integer(kind=c_int), value :: length_config_file_name
          end subroutine precicef_create

          ! Initialize - no longer returns time step size
          subroutine precicef_initialize()&
          &bind(c, name='precicef_initialize_')

            use, intrinsic :: iso_c_binding
          end subroutine precicef_initialize

          ! Advance - no longer returns time step size  
          subroutine precicef_advance(dt)&
          &bind(c, name='precicef_advance_')

            use, intrinsic :: iso_c_binding
            real(kind=c_double) :: dt
          end subroutine precicef_advance

          subroutine precicef_finalize()&
          &bind(c, name='precicef_finalize_')

            use, intrinsic :: iso_c_binding
          end subroutine precicef_finalize

          ! Get max time step size (replaces return value from initialize/advance)
          subroutine precicef_get_max_time_step_size(max_dt)&
          &bind(c, name='precicef_get_max_time_step_size_')

            use, intrinsic :: iso_c_binding
            real(kind=c_double) :: max_dt
          end subroutine precicef_get_max_time_step_size

          subroutine precicef_is_coupling_ongoing(ongoing)&
          &bind(c, name='precicef_is_coupling_ongoing_')

            use, intrinsic :: iso_c_binding
            integer(kind=c_int) :: ongoing
          end subroutine precicef_is_coupling_ongoing

          subroutine precicef_is_time_window_complete(is_complete)&
          &bind(c, name='precicef_is_time_window_complete_')

            use, intrinsic :: iso_c_binding
            integer(kind=c_int) :: is_complete
          end subroutine precicef_is_time_window_complete

          ! Get mesh dimensions (replaces getDimensions)
          subroutine precicef_get_mesh_dimensions(mesh_name, dimensions, length_mesh_name)&
          &bind(c, name='precicef_get_mesh_dimensions_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int)                  :: dimensions
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_get_mesh_dimensions

          ! Get data dimensions (new in v3)
          subroutine precicef_get_data_dimensions(mesh_name, data_name, dimensions,&
          &length_mesh_name, length_data_name)&
          &bind(c, name='precicef_get_data_dimensions_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            character(kind=c_char), dimension(*) :: data_name
            integer(kind=c_int)                  :: dimensions
            integer(kind=c_int), value           :: length_mesh_name
            integer(kind=c_int), value           :: length_data_name
          end subroutine precicef_get_data_dimensions

          ! Checkpoint functions (replace action-based API)
          subroutine precicef_requires_initial_data(is_required)&
          &bind(c, name='precicef_requires_initial_data_')

            use, intrinsic :: iso_c_binding
            integer(kind=c_int) :: is_required
          end subroutine precicef_requires_initial_data

          subroutine precicef_requires_reading_checkpoint(is_required)&
          &bind(c, name='precicef_requires_reading_checkpoint_')

            use, intrinsic :: iso_c_binding
            integer(kind=c_int) :: is_required
          end subroutine precicef_requires_reading_checkpoint

          subroutine precicef_requires_writing_checkpoint(is_required)&
          &bind(c, name='precicef_requires_writing_checkpoint_')

            use, intrinsic :: iso_c_binding
            integer(kind=c_int) :: is_required
          end subroutine precicef_requires_writing_checkpoint

          ! Mesh connectivity requirement check (name-based)
          subroutine precicef_requires_mesh_connectivity_for(mesh_name, required,&
          &length_mesh_name)&
          &bind(c, name='precicef_requires_mesh_connectivity_for_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int)                  :: required
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_requires_mesh_connectivity_for

          ! Gradient data requirement check (name-based)
          subroutine precicef_requires_gradient_data_for(mesh_name, data_name, required,&
          &length_mesh_name, length_data_name)&
          &bind(c, name='precicef_requires_gradient_data_for_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            character(kind=c_char), dimension(*) :: data_name
            integer(kind=c_int)                  :: required
            integer(kind=c_int), value           :: length_mesh_name
            integer(kind=c_int), value           :: length_data_name
          end subroutine precicef_requires_gradient_data_for

          ! Mesh vertex operations (name-based)
          subroutine precicef_set_vertex(mesh_name, position, vertex_id, length_mesh_name)&
          &bind(c, name='precicef_set_vertex_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            real(kind=c_double)                  :: position(*)
            integer(kind=c_int)                  :: vertex_id
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_vertex

          subroutine precicef_get_mesh_vertex_size(mesh_name, mesh_size, length_mesh_name)&
          &bind(c, name='precicef_get_mesh_vertex_size_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int)                  :: mesh_size
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_get_mesh_vertex_size

          subroutine precicef_set_vertices(mesh_name, mesh_size, positions, position_ids,&
          &length_mesh_name)&
          &bind(c, name='precicef_set_vertices_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int), value           :: mesh_size
            real(kind=c_double)                  :: positions(*)
            integer(kind=c_int)                  :: position_ids(*)
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_vertices

          subroutine precicef_get_mesh_vertex_ids_and_coordinates(mesh_name, size, ids, coordinates,&
          &length_mesh_name)&
          &bind(c, name='precicef_get_mesh_vertex_ids_and_coordinates_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int), value           :: size
            integer(kind=c_int)                  :: ids(*)
            real(kind=c_double)                  :: coordinates(*)
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_get_mesh_vertex_ids_and_coordinates

          ! Mesh connectivity (vertex-based API)
          subroutine precicef_set_edge(mesh_name, first_vertex_id, second_vertex_id,&
          &length_mesh_name)&
          &bind(c, name='precicef_set_edge_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int)                  :: first_vertex_id
            integer(kind=c_int)                  :: second_vertex_id
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_edge

          subroutine precicef_set_triangle(mesh_name, first_vertex_id, second_vertex_id,&
          &third_vertex_id, length_mesh_name)&
          &bind(c, name='precicef_set_triangle_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int)                  :: first_vertex_id
            integer(kind=c_int)                  :: second_vertex_id
            integer(kind=c_int)                  :: third_vertex_id
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_triangle

          subroutine precicef_set_quad(mesh_name, first_vertex_id, second_vertex_id,&
          &third_vertex_id, fourth_vertex_id, length_mesh_name)&
          &bind(c, name='precicef_set_quad_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int)                  :: first_vertex_id
            integer(kind=c_int)                  :: second_vertex_id
            integer(kind=c_int)                  :: third_vertex_id
            integer(kind=c_int)                  :: fourth_vertex_id
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_quad

          subroutine precicef_set_tetrahedron(mesh_name, first_vertex_id, second_vertex_id,&
          &third_vertex_id, fourth_vertex_id, length_mesh_name)&
          &bind(c, name='precicef_set_tetrahedron')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int)                  :: first_vertex_id
            integer(kind=c_int)                  :: second_vertex_id
            integer(kind=c_int)                  :: third_vertex_id
            integer(kind=c_int)                  :: fourth_vertex_id
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_tetrahedron

          ! Bulk connectivity functions (new in v3)
          subroutine precicef_set_mesh_edges(mesh_name, size, vertices, length_mesh_name)&
          &bind(c, name='precicef_set_mesh_edges_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int), value           :: size
            integer(kind=c_int)                  :: vertices(*)
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_mesh_edges

          subroutine precicef_set_mesh_triangles(mesh_name, size, vertices, length_mesh_name)&
          &bind(c, name='precicef_set_mesh_triangles_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int), value           :: size
            integer(kind=c_int)                  :: vertices(*)
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_mesh_triangles

          subroutine precicef_set_mesh_quads(mesh_name, size, vertices, length_mesh_name)&
          &bind(c, name='precicef_set_mesh_quads_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int), value           :: size
            integer(kind=c_int)                  :: vertices(*)
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_mesh_quads

          subroutine precicef_set_mesh_tetrahedra(mesh_name, size, vertices, length_mesh_name)&
          &bind(c, name='precicef_set_mesh_tetrahedra_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int), value           :: size
            integer(kind=c_int)                  :: vertices(*)
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_mesh_tetrahedra

          ! Unified data access (replaces all read/write block/scalar/vector data functions)
          subroutine precicef_read_data(mesh_name, data_name, vertex_ids, size,&
          &relative_read_time, data_values, length_mesh_name, length_data_name)&
          &bind(c, name='precicef_read_data_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            character(kind=c_char), dimension(*) :: data_name
            integer(kind=c_int)                  :: vertex_ids(*)
            integer(kind=c_int), value           :: size
            real(kind=c_double)                  :: relative_read_time
            real(kind=c_double)                  :: data_values(*)
            integer(kind=c_int), value           :: length_mesh_name
            integer(kind=c_int), value           :: length_data_name
          end subroutine precicef_read_data

          subroutine precicef_write_data(mesh_name, data_name, vertex_ids, size,&
          &data_values, length_mesh_name, length_data_name)&
          &bind(c, name='precicef_write_data_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            character(kind=c_char), dimension(*) :: data_name
            integer(kind=c_int)                  :: vertex_ids(*)
            integer(kind=c_int), value           :: size
            real(kind=c_double)                  :: data_values(*)
            integer(kind=c_int), value           :: length_mesh_name
            integer(kind=c_int), value           :: length_data_name
          end subroutine precicef_write_data

          ! Unified gradient data access
          subroutine precicef_write_gradient_data(mesh_name, data_name, vertex_ids, size,&
          &gradient_values, length_mesh_name, length_data_name)&
          &bind(c, name='precicef_write_gradient_data_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            character(kind=c_char), dimension(*) :: data_name
            integer(kind=c_int)                  :: vertex_ids(*)
            integer(kind=c_int), value           :: size
            real(kind=c_double)                  :: gradient_values(*)
            integer(kind=c_int), value           :: length_mesh_name
            integer(kind=c_int), value           :: length_data_name
          end subroutine precicef_write_gradient_data

          ! Direct mesh access
          subroutine precicef_set_mesh_access_region(mesh_name, bounding_box, size,&
          &length_mesh_name)&
          &bind(c, name='precicef_set_mesh_access_region_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            real(kind=c_double)                  :: bounding_box(*)
            integer(kind=c_int), value           :: size
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_set_mesh_access_region

          ! Time interpolation functions
          subroutine precicef_map_and_read_data(mesh_name, data_name, vertex_ids, size,&
          &relative_read_time, data_values, length_mesh_name, length_data_name)&
          &bind(c, name='precicef_map_and_read_data_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            character(kind=c_char), dimension(*) :: data_name
            integer(kind=c_int)                  :: vertex_ids(*)
            integer(kind=c_int), value           :: size
            real(kind=c_double)                  :: relative_read_time
            real(kind=c_double)                  :: data_values(*)
            integer(kind=c_int), value           :: length_mesh_name
            integer(kind=c_int), value           :: length_data_name
          end subroutine precicef_map_and_read_data

          subroutine precicef_write_and_map_data(mesh_name, data_name, vertex_ids, size,&
          &data_values, length_mesh_name, length_data_name)&
          &bind(c, name='precicef_write_and_map_data_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            character(kind=c_char), dimension(*) :: data_name
            integer(kind=c_int)                  :: vertex_ids(*)
            integer(kind=c_int), value           :: size
            real(kind=c_double)                  :: data_values(*)
            integer(kind=c_int), value           :: length_mesh_name
            integer(kind=c_int), value           :: length_data_name
          end subroutine precicef_write_and_map_data

          ! Version information
          subroutine precicef_get_version_information(version_info, length_version_info)&
          &bind(c, name="precicef_get_version_information_")
            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: version_info
            integer(kind=c_int), value :: length_version_info
          end subroutine precicef_get_version_information

          ! Reset mesh
          subroutine precicef_reset_mesh(mesh_name, length_mesh_name)&
          &bind(c, name='precicef_reset_mesh_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: mesh_name
            integer(kind=c_int), value           :: length_mesh_name
          end subroutine precicef_reset_mesh

          ! Profiling functions
          subroutine precicef_start_profiling_section(section_name, length_section_name)&
          &bind(c, name='precicef_start_profiling_section_')

            use, intrinsic :: iso_c_binding
            character(kind=c_char), dimension(*) :: section_name
            integer(kind=c_int), value           :: length_section_name
          end subroutine precicef_start_profiling_section

          subroutine precicef_stop_last_profiling_section()&
          &bind(c, name='precicef_stop_last_profiling_section_')

            use, intrinsic :: iso_c_binding
          end subroutine precicef_stop_last_profiling_section

        end interface
#endif
      end module precice_mod
