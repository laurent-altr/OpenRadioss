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
      contains
        SUBROUTINE FILL_VOXEL(flag, &
        &  nsn,&
        &  nsnr,&
        &  nbx,&
        &  nby,&
        &  nbz,&
        &  nrtm,&
        &  s_xrem,&
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
        & xrem,&
        & box_limit_main)
          USE CONSTANT_MOD
!-----------------------------------------------
          implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in), value :: flag
          integer, intent(in), value :: nsn
          integer, intent(in), value :: nsnr
          integer, intent(in), value :: nbx
          integer, intent(in), value :: nby
          integer, intent(in), value :: nbz
          integer, intent(in), value :: nrtm
          integer, intent(in), value :: s_xrem
          integer, intent(in), value :: numnod
          integer, intent(in) :: nsv(nsn)
          integer, intent(inout) :: voxel((nbx+2)*(nby+2)*(nbz+2))
          integer, dimension(:), allocatable, intent(inout) :: next_nod
          integer, intent(inout) :: size_nod
          integer, intent(inout) :: nb_voxel_on
          integer, dimension(:), allocatable, intent(inout) :: list_nb_voxel_on
          my_real, intent(in) :: x(3,numnod)
          my_real, intent(in) :: stfn(nsn)
          my_real, intent(in) :: xrem(s_xrem,nsnr)
          my_real, intent(in) :: box_limit_main(12)
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
            if(flag == FLAG_LOCAL) then
              nb_voxel_on = 0
              size_nod = nsn + nsnr
              allocate(last_nod(size_nod))
              allocate(next_nod(size_nod))
              allocate(list_nb_voxel_on(size_nod))
              write(6,*) 'nsn+nsnr',nsn+nsnr

              do i=1,nsn
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
            elseif(flag == FLAG_REMOTE) then
             write(6,*) 'nsn+nsnr',nsn+nsnr


!=======================================================================
! 2   Add remote (spmd) nodes to the cells
!=======================================================================
              do j = 1, nsnr
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
              deallocate(last_nod)
            endif
          endif
        END SUBROUTINE
      END MODULE FILL_VOXEL_MOD
