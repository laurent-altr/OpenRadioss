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
      MODULE FILL_VOXEL_MOD
        integer, parameter :: FLAG_REMOTE = 1
        integer, parameter :: FLAG_LOCAL = 0
        integer, parameter :: FLAG_NONE = -1
      contains
        SUBROUTINE FILL_VOXEL_LOCAL(&
        &  istart,&
        &  nsn,&
        &  nsnr,&
        &  nbx,&
        &  nby,&
        &  nbz,&
        &  nrtm,&
        &  numnod,&
        & nsv,&
        & voxel,&
        & next_nod,&
        & size_nod, &
        & nb_voxel_on,&
        & list_nb_voxel_on,&
        & last_nod, &
        & x,&
        & stfn,&
        & box_limit_main)
          USE CONSTANT_MOD
          USE EXTEND_ARRAY_MOD, ONLY : extend_array
!-----------------------------------------------
          implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in), value :: istart !< starting index 
          integer, intent(in), value :: nsn !< number of local secondary nodes
          integer, intent(in), value :: nsnr !< number of remote secondary nodes
          integer, intent(in), value :: nbx !< number of cells in x direction
          integer, intent(in), value :: nby !< number of cells in y direction
          integer, intent(in), value :: nbz !< number of cells in z direction
          integer, intent(in), value :: nrtm !< number of segments (rectangles) 
          integer, intent(in), value :: numnod !< total number of nodes
          integer, intent(in) :: nsv(nsn) !< secondary node list to global node list
          integer, intent(inout) :: voxel((nbx+2)*(nby+2)*(nbz+2)) !< voxel data structure
          integer, dimension(:), allocatable, intent(inout) :: next_nod !< next node in the voxel
          integer, intent(inout) :: size_nod !< size of the nod arrays
          integer, intent(inout) :: nb_voxel_on !< number of voxels with nodes
          integer, dimension(:), allocatable, intent(inout) :: list_nb_voxel_on !< list of voxels with nodes
          my_real, intent(in) :: x(3,numnod) !< global node coordinates
          my_real, intent(in) :: stfn(nsn) !< secondary node stiffness
          my_real, intent(in) :: box_limit_main(12) !< bounding box of the main segments 
          integer, dimension(:), allocatable, intent(inout) :: last_nod

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,j
          my_real :: xmin, xmax, ymin, ymax, zmin, zmax
          my_real :: xminb, xmaxb, yminb, ymaxb, zminb, zmaxb
          integer :: ix, iy, iz
          integer :: first, last
          integer :: cellid

! The global bounding box contains all the nodes
! Some nodes may be highly distant from the impact zone
! The domain is subdivided into cells (voxel)
! All the cells have the same size, except the first and the last one in each direction

! Bounding box of the model
          xmin = box_limit_main(4)
          ymin = box_limit_main(5)
          zmin = box_limit_main(6)
          xmax = box_limit_main(1)
          ymax = box_limit_main(2)
          zmax = box_limit_main(3)

! reduced bounding box of the model
! The reduced bounding box corresponds to voxel(2:nbx+1,2:nby+1,2:nbz+1), it contains cells of the same size
          xminb = box_limit_main(10)
          yminb = box_limit_main(11)
          zminb = box_limit_main(12)
          xmaxb = box_limit_main(7)
          ymaxb = box_limit_main(8)
          zmaxb = box_limit_main(9)

!=======================================================================
! 1   Add local nodes to the cells
!=======================================================================

          if(nrtm > 0)then
            if(.not. allocated(last_nod)) size_nod = 0
            if(.not. allocated(next_nod)) size_nod = 0
            if(.not. allocated(list_nb_voxel_on)) nb_voxel_on = 0
              nb_voxel_on = 0
              call extend_array(last_nod, size_nod,nsn+nsnr)
              call extend_array(next_nod, size_nod ,nsn+nsnr)
              call extend_array(list_nb_voxel_on,size_nod,nsn+nsnr)
              list_nb_voxel_on = 0
              size_nod = max(size_nod,nsn+nsnr)
              if(nsn + nsnr > 0) then
                last_nod(1:nsn+nsnr) = 0
                next_nod(1:nsn+nsnr) = 0
                list_nb_voxel_on(1:nsn+nsnr) = 0
              endif

              do i=istart,nsn
                if(stfn(i) == zero)cycle
                j=nsv(i)
                if(x(1,j) < xmin)  cycle
                if(x(1,j) > xmax)  cycle
                if(x(2,j) < ymin)  cycle
                if(x(2,j) > ymax)  cycle
                if(x(3,j) < zmin)  cycle
                if(x(3,j) > zmax)  cycle
                ix=int(nbx*(x(1,j)-xminb)/(xmaxb-xminb))
                iy=int(nby*(x(2,j)-yminb)/(ymaxb-yminb))
                iz=int(nbz*(x(3,j)-zminb)/(zmaxb-zminb))
                ix=max(1,2+min(nbx,ix))
                iy=max(1,2+min(nby,iy))
                iz=max(1,2+min(nbz,iz))
                cellid = (iz-1)*(nbx+2)*(nby+2)+(iy-1)*(nbx+2)+ix
                first = voxel(cellid)
                if(first == 0)then
                  nb_voxel_on = nb_voxel_on + 1
                  list_nb_voxel_on(nb_voxel_on) = cellid
                  voxel(cellid) = i ! first
                  next_nod(i) = 0 ! last one
                  last_nod(i) = 0 ! no last
                elseif(last_nod(first) == 0)then
                  next_nod(first) = i ! next
                  last_nod(first) = i ! last
                  next_nod(i)     = 0 ! last one
                else
                  last = last_nod(first) ! last node in this voxel
                  next_nod(last)  = i ! next
                  last_nod(first) = i ! last
                  next_nod(i)     = 0 ! last one
                endif
              enddo
          endif !< nrtm
        END SUBROUTINE
        SUBROUTINE FILL_VOXEL_REMOTE( &
        & istart,&
        & iend,&
        & nsn,&
        & nsnr,&
        & nbx,&
        & nby,&
        & nbz,&
        & s_xrem,&
        & voxel,&
        & next_nod,&
        & size_nod, &
        & nb_voxel_on,&
        & list_nb_voxel_on,&
        & last_nod, &
        & xrem,&
        & box_limit_main)
          USE CONSTANT_MOD
          USE EXTEND_ARRAY_MOD, ONLY : extend_array
!-----------------------------------------------
          implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in), value :: istart !< starting index 
          integer, intent(in), value :: iend !< ending index
          integer, intent(in), value :: nsn !< number of local secondary nodes
          integer, intent(in), value :: nsnr !< number of remote secondary nodes
          integer, intent(in), value :: nbx !< number of cells in x direction
          integer, intent(in), value :: nby !< number of cells in y direction
          integer, intent(in), value :: nbz !< number of cells in z direction
          integer, intent(in), value :: s_xrem !< number of double data for remote nodes
          integer, intent(inout) :: voxel((nbx+2)*(nby+2)*(nbz+2)) !< voxel data structure
          integer, dimension(:), allocatable, intent(inout) :: next_nod !< next node in the voxel
          integer, intent(inout) :: size_nod !< size of the nod arrays
          integer, intent(inout) :: nb_voxel_on !< number of voxels with nodes
          integer, dimension(:), allocatable, intent(inout) :: list_nb_voxel_on !< list of voxels with nodes
          my_real, intent(in) :: xrem(s_xrem,nsnr) !< remote node data
          my_real, intent(in) :: box_limit_main(12) !< bounding box of the main segments 
          integer, dimension(:), allocatable, intent(inout) :: last_nod
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,j
          my_real :: xmin, xmax, ymin, ymax, zmin, zmax
          my_real :: xminb, xmaxb, yminb, ymaxb, zminb, zmaxb
          integer :: ix, iy, iz
          integer :: first, last
          integer :: cellid

! The global bounding box contains all the nodes
! Some nodes may be highly distant from the impact zone
! The domain is subdivided into cells (voxel)
! All the cells have the same size, except the first and the last one in each direction

! Bounding box of the model
          xmin = box_limit_main(4)
          ymin = box_limit_main(5)
          zmin = box_limit_main(6)
          xmax = box_limit_main(1)
          ymax = box_limit_main(2)
          zmax = box_limit_main(3)

! reduced bounding box of the model
! The reduced bounding box corresponds to voxel(2:nbx+1,2:nby+1,2:nbz+1), it contains cells of the same size
          xminb = box_limit_main(10)
          yminb = box_limit_main(11)
          zminb = box_limit_main(12)
          xmaxb = box_limit_main(7)
          ymaxb = box_limit_main(8)
          zmaxb = box_limit_main(9)

!=======================================================================
! 1   Add local nodes to the cells
!=======================================================================
              call extend_array(last_nod, size_nod ,nsn+nsnr)
              call extend_array(next_nod, size_nod ,nsn+nsnr)
              call extend_array(list_nb_voxel_on, nb_voxel_on,nsn+nsnr)
              size_nod = max(size_nod,nsn+nsnr)
!=======================================================================
! 2   Add remote (spmd) nodes to the cells
!=======================================================================
              do j = istart, iend
                if(xrem(1,j) < xmin)  cycle
                if(xrem(1,j) > xmax)  cycle
                if(xrem(2,j) < ymin)  cycle
                if(xrem(2,j) > ymax)  cycle
                if(xrem(3,j) < zmin)  cycle
                if(xrem(3,j) > zmax)  cycle
                ix=int(nbx*(xrem(1,j)-xminb)/(xmaxb-xminb))
                iy=int(nby*(xrem(2,j)-yminb)/(ymaxb-yminb))
                iz=int(nbz*(xrem(3,j)-zminb)/(zmaxb-zminb))
                ix=max(1,2+min(nbx,ix))
                iy=max(1,2+min(nby,iy))
                iz=max(1,2+min(nbz,iz))

                cellid = (iz-1)*(nbx+2)*(nby+2)+(iy-1)*(nbx+2)+ix

                first = voxel(cellid)

                if(first == 0)then
                  nb_voxel_on = nb_voxel_on + 1
                  list_nb_voxel_on( nb_voxel_on ) = cellid
                  voxel(cellid) = nsn+j ! first
                  next_nod(nsn+j)     = 0 ! last one
                  last_nod(nsn+j)     = 0 ! no last
                elseif(last_nod(first) == 0)then
                  next_nod(first) = nsn+j  ! next
                  last_nod(first) = nsn+j  ! last
                  next_nod(nsn+j)  = 0     ! last one
                else
                  last = last_nod(first)  ! last node in this voxel
                  next_nod(last)  = nsn+j ! next
                  last_nod(first) = nsn+j ! last
                  next_nod(nsn+j)     = 0 ! last one
                endif
              enddo
              !deallocate(last_nod)
        END SUBROUTINE FILL_VOXEL_REMOTE

      END MODULE FILL_VOXEL_MOD
