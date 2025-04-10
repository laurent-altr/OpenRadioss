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

! ----------------------------------------------------------------------------
    module cluster_node_mod
    integer, parameter :: UNCLASSIFIED = -1
    integer, parameter :: MIN_PATCH_SIZE = 100
    interface
        function cluster_nodes(coords, numnod, color, eps, minPts) bind(C, name="cluster_nodes")
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), value :: numnod, minPts
            real(c_double), intent(in) :: coords(*)
            integer(c_int), intent(out) :: color(*)
            real(c_double), value :: eps
            integer(c_int) :: cluster_nodes
        end function
        
    end interface

    !< Cluster node data structure, for local and remote clusters of nodes 
    type cluster_node_
        !                          DIR, MIN/MAX 
        double precision, dimension(3,2) :: bounding_box !< bounding box of the cluster 
        integer, dimension(:), allocatable :: index_to_win !< mapping from the node number to the window number
        integer, dimension(:), allocatable :: index_in_x !< mapping from the node number to the nsv number
        integer, dimension(:), allocatable :: index_in_nsv !< mapping from the node number to the nsv number
        integer :: numnod !< number of nodes in the cluster
        double precision :: volume 
    end type cluster_node_

    type cluster_mapping_
!       integer, dimension(:), allocatable :: nsv2win !< mapping from the node number to the window number
        integer :: ncolors
        integer, dimension(:), allocatable :: color !< cluster for each node
        type(cluster_node_), dimension(:), allocatable :: clusters
        integer :: nb_clusters
    end type cluster_mapping_

    contains

    ! computes the volume from the bounding box
    subroutine compute_cluster_volume(patchs,nspmd)
        implicit none
! dummy arguments
        integer, intent(in) :: nspmd
        type(cluster_mapping_) :: patchs(nspmd)

! local variables
        integer :: p,k 
        double precision :: dx, dy, dz
        double precision :: vol

        do p = 1, nspmd
        do k = 1, patchs(p)%nb_clusters
          dx = patchs(p)%clusters(k)%bounding_box(1,2) - patchs(p)%clusters(k)%bounding_box(1,1)
          dy = patchs(p)%clusters(k)%bounding_box(2,2) - patchs(p)%clusters(k)%bounding_box(2,1)
          dz = patchs(p)%clusters(k)%bounding_box(3,2) - patchs(p)%clusters(k)%bounding_box(3,1)
          vol = dx * dy * dz
          patchs(p)%clusters(k)%volume = vol
        end do
        enddo

    end subroutine compute_cluster_volume


    end module cluster_node_mod
    