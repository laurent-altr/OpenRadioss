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

    module init_patch_mod
        contains
    subroutine init_patch(patchs, eps, nodes, nspmd, rank, intbuf_tab,nsn)
        use cluster_node_mod
        use nodal_arrays_mod
        use intbufdef_mod
        implicit none
! dummy arguments
        integer, intent(in) :: nspmd
        integer, intent(in) :: nsn
        type(cluster_mapping_) :: patchs(nspmd)
        type(nodal_arrays_), intent(in) :: nodes
        type(intbuf_struct_) :: intbuf_tab
        double precision, intent(in) :: eps
        integer, intent(in) :: rank
! local variables
        integer :: ncolors
        integer :: i,j, k, n
        integer, dimension(:), allocatable :: counter
        double precision, dimension(:,:), allocatable :: coords
        integer, dimension(:), allocatable :: local2global
        integer :: n_local
        n_local = 0
        allocate(local2global(nsn))
        ! overestimation
        allocate(coords(3,nsn))

        local2global = UNCLASSIFIED
        n_local = 0
        do i = 1, nsn
            n = intbuf_tab%nsv(i)
            if( nodes%weight(n) == 1) then
                n_local = n_local + 1
                local2global(n_local) = i
                coords(1,n_local) = nodes%X(1,n)
                coords(2,n_local) = nodes%X(2,n)
                coords(3,n_local) = nodes%X(3,n)
            endif 
        enddo
          allocate(patchs(rank+1)%color(n_local))
          patchs(rank+1)%color(1:n_local) = UNCLASSIFIED
          if(n_local > 2*MIN_PATCH_SIZE) then
            ncolors =  cluster_nodes(coords,n_local,patchs(rank+1)%color , eps, MIN_PATCH_SIZE) 
          else if(n_local > 0) then
            ncolors = 1
            patchs(rank+1)%color(1:n_local) = 0
          else
            ncolors = 0
          end if
          patchs(rank+1)%nb_clusters = ncolors
          allocate(counter(ncolors))
          allocate(patchs(rank+1)%clusters(ncolors))
        
          counter = 0
          do i = 1, n_local
            k = patchs(rank+1)%color(i) + 1 ! C to Fortran indexing
            if(k > 0) then
              counter(k) = counter(k) + 1
              patchs(rank+1)%clusters(k)%numnod = counter(k)
            endif
          end do

          do k = 1, ncolors
            i = patchs(rank+1)%clusters(k)%numnod
            if(i > 0) then
              allocate(patchs(rank+1)%clusters(k)%index_to_win(i))
              allocate(patchs(rank+1)%clusters(k)%index_in_x(i))
              allocate(patchs(rank+1)%clusters(k)%index_in_nsv(i))

              ! initialize to -HUGE
                patchs(rank+1)%clusters(k)%index_to_win(1:i) = -HUGE(i)
            endif
            j = 0
            do i = 1, n_local
              if(patchs(rank+1)%color(i) == k - 1) then
                j = j + 1
                patchs(rank+1)%clusters(k)%index_to_win(j) = i
                patchs(rank+1)%clusters(k)%index_in_x(j) = intbuf_tab%nsv(local2global(i))
                patchs(rank+1)%clusters(k)%index_in_nsv(j) = local2global(i)
              endif
            end do
            write(6,*) 'cluster', k, 'numnod', patchs(rank+1)%clusters(k)%numnod
            !write(6,*) 'cluster', k, 'index_to_win', patchs(rank+1)%clusters(k)%index_to_win(1:patchs(rank+1)%clusters(k)%numnod)
          end do


          deallocate(counter)
          deallocate(coords)
          deallocate(local2global)

    end subroutine init_patch
    subroutine init_patch_m(patchs, eps, nodes,  intbuf_tab,nmn)
        use cluster_node_mod
        use nodal_arrays_mod
        use intbufdef_mod
        implicit none
! dummy arguments
        integer, intent(in) :: nmn
        type(cluster_mapping_) :: patchs
        type(nodal_arrays_), intent(in) :: nodes
        type(intbuf_struct_) :: intbuf_tab
        double precision, intent(in) :: eps
! local variables
        integer :: ncolors
        integer :: i,j, k, n
        integer, dimension(:), allocatable :: counter
        double precision, dimension(:,:), allocatable :: coords
        integer, dimension(:), allocatable :: local2global
        integer :: n_local
        n_local = 0
        allocate(local2global(nmn))
        ! overestimation
        allocate(coords(3,nmn))

        local2global = UNCLASSIFIED
        n_local = 0
        do i = 1, nmn
            n = intbuf_tab%msr(i)
            if(n > 0 ) then
            if( nodes%weight(n) == 1) then
                n_local = n_local + 1
                local2global(n_local) = i
                coords(1,n_local) = nodes%X(1,n)
                coords(2,n_local) = nodes%X(2,n)
                coords(3,n_local) = nodes%X(3,n)
            endif 
          endif
        enddo
          allocate(patchs%color(n_local))
          patchs%color(1:n_local) = UNCLASSIFIED
          if(n_local > 2*MIN_PATCH_SIZE) then
            ncolors =  cluster_nodes(coords,n_local,patchs%color , eps, MIN_PATCH_SIZE) 
          else if(n_local > 0) then
            ncolors = 1
            patchs%color(1:n_local) = 0
          else
            ncolors = 0
          end if
          patchs%nb_clusters = ncolors
          allocate(counter(ncolors))
          allocate(patchs%clusters(ncolors))
        
          counter = 0
          do i = 1, n_local
            k = patchs%color(i) + 1 ! C to Fortran indexing
            if(k > 0) then
              counter(k) = counter(k) + 1
              patchs%clusters(k)%numnod = counter(k)
            endif
          end do

          do k = 1, ncolors
            i = patchs%clusters(k)%numnod
            if(i > 0) then
              allocate(patchs%clusters(k)%index_in_x(i))
              allocate(patchs%clusters(k)%index_in_nsv(i))
            endif
            j = 0
            do i = 1, n_local
              if(patchs%color(i) == k - 1) then
                j = j + 1
                patchs%clusters(k)%index_in_x(j) = intbuf_tab%msr(local2global(i))
                patchs%clusters(k)%index_in_nsv(j) = local2global(i)
              endif
            end do
            write(6,*) 'main cluster', k, 'numnod', patchs%clusters(k)%numnod
            !write(6,*) 'cluster', k, 'index_to_win', patchs(rank+1)%clusters(k)%index_to_win(1:patchs(rank+1)%clusters(k)%numnod)
          end do


          deallocate(counter)
          deallocate(coords)
          deallocate(local2global)

    end subroutine init_patch_m

    end module init_patch_mod


