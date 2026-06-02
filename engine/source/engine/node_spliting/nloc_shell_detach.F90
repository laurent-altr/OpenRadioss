!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    nloc_shell_detach_mod   ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||--- called by ------------------------------------------------------
!||    resol                   ../engine/source/engine/resol.F
!||====================================================================
      module nloc_shell_detach_mod
        implicit none

        real(kind=8), parameter :: nloc_crack_threshold = 1.0D0

      contains

!! \brief Crack propagation by node splitting driven by the non-local damage field.
!||====================================================================
!||    nloc_shell_detach         ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||--- calls      -----------------------------------------------------
!||    detach_node               ../engine/source/engine/node_spliting/detach_node.F90
!||    spmd_exchange_ghost_shells ../engine/source/engine/node_spliting/ghost_shells.F90
!||    stlsort_int_int            ../common_source/tools/sort/cppsort.cpp
!||--- uses       -----------------------------------------------------
!||    connectivity_mod           ../common_source/modules/connectivity.F90
!||    detach_node_mod            ../engine/source/engine/node_spliting/detach_node.F90
!||    ghost_shells_mod           ../engine/source/engine/node_spliting/ghost_shells.F90
!||    interfaces_mod             ../common_source/modules/interfaces/interfaces_mod.F90
!||    nlocal_reg_mod             ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod           ../common_source/modules/nodal_arrays.F90
!||    precision_mod              ../common_source/modules/precision_mod.F90
!||    spmd_mod                   ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine nloc_shell_detach(nodes, element, interf, npari, ninter, ipari, numnod, &
          numnodg, numelc, ispmd, nspmd, new_crack, nloc_dmg, nthread)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use spmd_mod
          use ghost_shells_mod
          use precision_mod, only : wp
          use connectivity_mod
          use nodal_arrays_mod
          use interfaces_mod
          use detach_node_mod
          use nlocal_reg_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes        !< nodal arrays
          type(connectivity_), intent(inout) :: element      !< element connectivity
          type(interfaces_),   intent(inout) :: interf       !< interface structure
          integer,             intent(in)    :: npari        !< number of interface parameters
          integer,             intent(in)    :: ninter       !< number of interfaces
          integer,             intent(inout) :: ipari(npari, ninter) !< interface parameters
          integer,             intent(inout) :: numnod       !< current number of local nodes
          integer,             intent(inout) :: numnodg      !< current number of global nodes
          integer,             intent(in)    :: numelc       !< number of quad-shell elements
          integer,             intent(in)    :: ispmd        !< local MPI rank (0-based)
          integer,             intent(in)    :: nspmd        !< number of MPI domains
          integer,             intent(out)   :: new_crack    !< number of new nodes created (sum over all domains)
          type(nlocal_str_),   intent(inout) :: nloc_dmg     !< non-local damage structure
          integer,             intent(in)    :: nthread      !< number of OpenMP threads
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=wp), allocatable :: detach_shell(:)  ! normalised damage per shell (0=intact, 1=failed)
          real(kind=wp), allocatable :: ghostshelldamage(:)
          double precision, allocatable :: nodal_damage(:)
          integer, allocatable :: shell_list(:)
          integer, allocatable :: detached_nodes_local(:), detached_nodes(:)
          integer, allocatable :: nb_detached_nodes(:), nb_detached_nodes_global(:)
          integer, allocatable :: permutation(:), processor(:), local_pos(:)
          logical, allocatable :: parent_split_step(:)  ! Guard: one split per parent per cycle
          integer :: i, j, k, n1, ii, p, parent_id
          integer :: numnod0, numnodg0, total_new_nodes, nb_detached_nodes_local
          integer :: shells_to_detach, nghostshells, old_max_uid
          integer :: crack(20), nl_idx, nl_dof
          integer :: n_debug_shells
          integer :: diag_nl_idx, diag_old_pos, diag_dof  ! for diagnostic output
          double precision :: v(3), distance, current_max_dist, ecc_norm
          real(kind=wp) :: shell_peak_damage
          integer :: displ(nspmd)
          real(kind=wp), parameter :: split_frac = 0.8_wp  ! fraction of threshold to trigger split
          logical, parameter :: debug_detach_monitor = .false.  ! Disable verbose debug output
          integer, save :: diag_call_count = 0  ! sequential counter across calls
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          new_crack = 0

          if (nloc_dmg%imod == 0) return  ! non-local damage inactive

          numnodg0 = numnodg


          ! this is a non-physical test-only implementation of node splitting, for demonstration purposes only
          ! The purpose is to show of to create a variable detach_shell(i) that contains a scalar value per shell, that can be used
          ! to trigger a node split when detach_shell(i) >= 1.0
          ! This variable detach_shell is known for all local shells, and is exchanged across MPI domains to update the ghost shells
          ! The ghost shell values are then used to update the nodal damage for all nodes connected to the ghost shells.

          ! Initialise per-shell distance-to-centre array (lazy, persists across cycles)
          if (.not. allocated(element%shell%damage)) then
            allocate(element%shell%damage(numelc))
            element%shell%damage = 0.0D0
            allocate(element%shell%dist_to_center(numelc))
            do i = 1, numelc
              n1 = element%shell%ixc(2, i)
              v(1) = (nodes%x(1, n1) + nodes%x(1, element%shell%ixc(3,i)) + &
                nodes%x(1, element%shell%ixc(4,i)) + nodes%x(1, element%shell%ixc(5,i))) / 4.0D0
              v(2) = (nodes%x(2, n1) + nodes%x(2, element%shell%ixc(3,i)) + &
                nodes%x(2, element%shell%ixc(4,i)) + nodes%x(2, element%shell%ixc(5,i))) / 4.0D0
              v(3) = (nodes%x(3, n1) + nodes%x(3, element%shell%ixc(3,i)) + &
                nodes%x(3, element%shell%ixc(4,i)) + nodes%x(3, element%shell%ixc(5,i))) / 4.0D0
              distance = 0.0D0
              do j = 1, 4
                distance = max(distance, sqrt( &
                  (v(1) - nodes%x(1, element%shell%ixc(j+1,i)))**2 + &
                  (v(2) - nodes%x(2, element%shell%ixc(j+1,i)))**2 + &
                  (v(3) - nodes%x(3, element%shell%ixc(j+1,i)))**2))
              end do
              element%shell%dist_to_center(i) = distance
            end do
          end if

          allocate(detach_shell(0:numelc))
          detach_shell = 0.0_wp
          do i = 1, numelc
            if (element%shell%damage(i) >= 0.99999_wp) then
              detach_shell(i) = 1.0_wp   ! already split in a previous cycle
              cycle
            end if
            do j = 1, 4
              ! Exclude corners that cannot be split: their high UNL must not
              ! block crack propagation at the shell's remaining corners.
              if (nodes%nchilds(nodes%parent_node(element%shell%ixc(j+1, i))) > 0) cycle
              nl_idx = nloc_dmg%idxi(element%shell%ixc(j+1, i))
              if (nl_idx > 0) then
                do nl_dof = nloc_dmg%posi(nl_idx), nloc_dmg%posi(nl_idx + 1) - 1
                  detach_shell(i) = max(detach_shell(i), &
                    real(nloc_dmg%unl(nl_dof), wp) / real(nloc_crack_threshold, wp))
                end do
              end if
            end do
            detach_shell(i) = min(detach_shell(i), 1.0_wp)
          end do

          ! Exchange ghost-shell damage values across MPI domains
          nghostshells = size(element%ghost_shell%nodes, 2)
          allocate(ghostshelldamage(nghostshells))
          ghostshelldamage = 0.0_wp

          ! The values in detach_shell are local to each MPI domain.
          ! in output, the ghostshelldamage array will contain the maximum detach_shell value for each ghost shell across all MPI domains.
          call spmd_exchange_ghost_shells(element, ispmd, nspmd, 1, detach_shell, ghostshelldamage)

          ! Accumulate nodal damage: max detach_shell of all connected shells
          numnod0 = numnod
          allocate(nodal_damage(numnod))
          nodal_damage = 0.0D0
          do i = 1, numelc
            if (detach_shell(i) > 0.9999D0) cycle
            do j = 1, 4
              n1 = element%shell%ixc(j+1, i)
              nodal_damage(n1) = max(nodal_damage(n1), real(detach_shell(i), 8))
            end do
          end do
          do i = 1, nghostshells
            element%ghost_shell%damage(i) = ghostshelldamage(i)
            if (ghostshelldamage(i) > 0.9999D0) cycle
            do j = 1, 4
              n1 = element%ghost_shell%nodes(j, i)
              if (n1 <= 0) cycle
              nodal_damage(n1) = max(nodal_damage(n1), real(ghostshelldamage(i), 8))
            end do
          end do
          deallocate(ghostshelldamage)

          nb_detached_nodes_local = 0
          allocate(detached_nodes_local(numnod))
          allocate(shell_list(numelc))
          shell_list = 0
          shells_to_detach = 0
          allocate(parent_split_step(numnod))
          parent_split_step = .false.

          ! test  PUNCH_NLOCAL
          block  ! example on how to detach a list of nodes and shells, should be replaced by a physical criterion based on the non-local damage field
            !detach nodes :
            !Nodes 10682 10683 10684 .. 10722
            ! from list
            !Elements 12963 12966 12967 12987 13044 13047 13048 13053 13054 13055 13071 13074 13075 13089 13092 13093 12891 12894 12895 12984 12912 129113 12981 12985
            ! 13323 13326 13327 13341 13344 13345 13395 13398 13399 13413 13416 13417 13623 13626 13627 13644 13695 13698 13699 13716 13717 13722 13723
            integer  :: node_list(41), local_node_list(41)
            integer :: shell_list(48), local_shell_list(48)
            integer :: i,j
            integer :: nlocal_shell, nlocal_node

            node_list = [10682, 10683, 10684, 10685, 10686, 10687, 10688, 10689, 10690, 10691, 10692, 10693, &
              10694, 10695, 10696, 10697, 10698, 10699, 10700, 10701, 10702, 10703, 10704, 10705, 10706, 10707, &
              10708, 10709, 10710, 10711, 10712, 10713, 10714, 10715, 10716, 10717, 10718, 10719, 10720, 10721, 10722]

            shell_list = [12963, 12966, 12967, 12987, 13044, 13047, 13048, 13053, 13054, 13055, 13071, 13074, 13075, 12984, &
              13089, 13092, 13093, 12891, 12894, 12895, 12909, 12912, 12913, 12981, 12985, 13323, 13326, 13327, 13341, 13344, 13345, &
              13395, 13398, 13399, 13413, 13416, 13417, 13623, 13626, 13627, 13644, 13695, 13698, 13699, 13716, 13717, 13722, 13723]

            nlocal_shell = 0
            nlocal_node = 0
            do i = 1, size(shell_list)
              j = get_local_shell_id(element%shell, shell_list(i))
              if(j <= 0) cycle
              if(element%shell%damage(j) >= 1.0D0) cycle
              nlocal_shell = nlocal_shell + 1
              local_shell_list(nlocal_shell) = j
              element%shell%damage(j) = 1.0D0
            end do
            do i = 1, 41
              j = get_local_node_id(nodes, node_list(i))
              if (j > 0)  then
                nlocal_node = nlocal_node + 1
                local_node_list(nlocal_node) = j
              end if
            end do

            if(nlocal_shell > 0 ) then
              do i = 1, nlocal_node
                ! extends the arrays, but does not set the all new nodes values, see after
                call detach_node(nodes, local_node_list(i), element, local_shell_list, nlocal_shell, &
                  npari, ninter, ipari, interf, nloc_dmg, nthread, nspmd, ispmd)
                numnod = numnod + 1
                if (ispmd == 0) numnodg = numnodg + 1
              enddo
            endif
          end block




          ! --------------------------------------------------------------------------
          ! Synchronise new-node UIDs across all MPI domains (same as test_jc version)
          ! --------------------------------------------------------------------------
          allocate(nb_detached_nodes(nspmd))
          allocate(nb_detached_nodes_global(nspmd))
          nb_detached_nodes_global = 0
          nb_detached_nodes = 0
          nb_detached_nodes(ispmd + 1) = numnod - numnod0
          if (nspmd > 1) then
            call spmd_allreduce(nb_detached_nodes, nb_detached_nodes_global, nspmd, SPMD_SUM)
          else
            nb_detached_nodes_global = nb_detached_nodes
          end if

          total_new_nodes = sum(nb_detached_nodes_global(1:nspmd))
          if (total_new_nodes > 0) new_crack = total_new_nodes

          allocate(detached_nodes(total_new_nodes))

          displ(1:nspmd) = 0
          do i = 2, nspmd
            displ(i) = displ(i-1) + nb_detached_nodes_global(i-1)
          end do
          if (nspmd > 1) then
            call spmd_allgatherv(detached_nodes_local, nb_detached_nodes_global(ispmd+1), &
              detached_nodes, nb_detached_nodes_global, displ)
          else
            detached_nodes(1:total_new_nodes) = detached_nodes_local(1:total_new_nodes)
          end if

          if (nspmd > 1) then
            call spmd_allreduce(numnodg0, p, 1, SPMD_MAX)
            numnodg0 = p
          end if
          if (nspmd > 1) then
            call spmd_allreduce(nodes%max_uid, old_max_uid, 1, SPMD_MAX)
          else
            old_max_uid = nodes%max_uid
          end if

          k = sum(nb_detached_nodes_global(1:nspmd))
          allocate(processor(k))
          allocate(local_pos(k))
          k = 0
          do p = 1, nspmd
            do i = 1, nb_detached_nodes_global(p)
              k = k + 1
              processor(k) = p
              if (ispmd + 1 == p) then
                local_pos(k) = i
              else
                local_pos(k) = 0
              end if
            end do
          end do

          k = sum(nb_detached_nodes_global(1:nspmd))
          allocate(permutation(k))
          do i = 1, k
            permutation(i) = i
          end do
          call stlsort_int_int(k, detached_nodes, permutation)

          do ii = 1, k
            i = permutation(ii)
            p = processor(i)
            old_max_uid = old_max_uid + 1
            numnodg0 = numnodg0 + 1
            if (p == ispmd + 1) then
              j = local_pos(i)
              nodes%itab(numnod0 + j)         = old_max_uid
              nodes%itabm1(numnod0 + j)       = old_max_uid
              nodes%itabm1(2*(numnod0 + j))   = numnod0 + j
              nodes%nodglob(numnod0 + j)      = numnodg0
            end if
            j = get_local_node_id(nodes, detached_nodes(i))
            if (j > 0) then
              ! The owning domain already halved ms/ms0 inside set_new_node_values
              ! (called from detach_node).  Only halve here for boundary copies on
              ! non-owning MPI domains that never called detach_node.
              if (p /= ispmd + 1) then
                nodes%ms(j)  = nodes%ms(j)  / 2.0_wp
                nodes%ms0(j) = nodes%ms0(j) / 2.0_wp
              end if
              nodes%nchilds(nodes%parent_node(j)) = nodes%nchilds(nodes%parent_node(j)) + 1
            end if
          end do

          nodes%max_uid = old_max_uid
          numnodg = numnodg0

          if (allocated(permutation))            deallocate(permutation)
          if (allocated(processor))              deallocate(processor)
          if (allocated(local_pos))              deallocate(local_pos)
          if (allocated(nb_detached_nodes))      deallocate(nb_detached_nodes)
          if (allocated(nb_detached_nodes_global)) deallocate(nb_detached_nodes_global)
          if (allocated(detached_nodes_local))   deallocate(detached_nodes_local)
          if (allocated(detached_nodes))         deallocate(detached_nodes)
          if (allocated(detach_shell))           deallocate(detach_shell)
          if (allocated(shell_list))             deallocate(shell_list)
          if (allocated(nodal_damage))           deallocate(nodal_damage)
          if (allocated(parent_split_step))      deallocate(parent_split_step)

        end subroutine nloc_shell_detach

      end module nloc_shell_detach_mod
