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
 
       SUBROUTINE FILL_VOXEL_LOCAL_PARTIAL(&
        & nsn,&
        & nsv,&
        & nsnr,&
        & nrtm,&
        & numnod,&
        & x,&
        & stfn,&
        & s, &
        & requests, &
        & nrequests)
          USE INTER_STRUCT_MOD
          USE CONSTANT_MOD
          USE EXTEND_ARRAY_MOD, ONLY : extend_array
!-----------------------------------------------
          implicit none
#include "my_real.inc"
#ifdef MPI
#include "mpif.h"
#endif
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in), value :: nsn !< number of local secondary nodes
          integer, intent(in) :: nsv(nsn) !< secondary node list to global node list 
          integer, intent(in), value :: nsnr !< number of remote secondary nodes
          integer, intent(in), value :: nrtm !< number of segments (rectangles) 
          integer, intent(in), value :: numnod !< total number of nodes
          my_real, intent(in) :: x(3,numnod) !< global node coordinates
          my_real, intent(in) :: stfn(nsn) !< secondary node stiffness
          TYPE(inter_struct_type), intent(inout) :: s !< structure for interface sorting comm
          integer, intent(in), value :: nrequests !< number of requests
          integer, intent(inout) :: requests(nrequests) !< MPI request

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,j
          my_real :: xmin, xmax, ymin, ymax, zmin, zmax
          my_real :: xminb, xmaxb, yminb, ymaxb, zminb, zmaxb
          integer :: ix, iy, iz
          integer :: first, last
          integer :: cellid
          integer, parameter :: chunk = 32
          integer :: nchunks
          integer :: chunk_size
          integer :: ic, k
          integer :: flag
          integer :: ierr
          flag = 0

! The global bounding box contains all the nodes
! Some nodes may be highly distant from the impact zone
! The domain is subdivided into cells (voxel)
! All the cells have the same size, except the first and the last one in each direction

! Bounding box of the model
          xmin = s%box_limit_main(4)
          ymin = s%box_limit_main(5)
          zmin = s%box_limit_main(6)
          xmax = s%box_limit_main(1)
          ymax = s%box_limit_main(2)
          zmax = s%box_limit_main(3)

! reduced bounding box of the model
! The reduced bounding box corresponds to voxel(2:s%nbx+1,2:s%nby+1,2:s%nbz+1), it contains cells of the same size
          xminb = s%box_limit_main(10)
          yminb = s%box_limit_main(11)
          zminb = s%box_limit_main(12)
          xmaxb = s%box_limit_main(7)
          ymaxb = s%box_limit_main(8)
          zmaxb = s%box_limit_main(9)

!=======================================================================
! 1   Add local nodes to the cells
!=======================================================================

          if(nrtm > 0)then
           !if((s%istart-1)*chunk+1 < nsn)write(6,*) 'start',(s%istart-1)*chunk+1,"nsn=",nsn,"nsnr=",nsnr

            if(s%istart == 1) then
            if(.not. allocated(s%last_nod)) s%size_node = 0
            if(.not. allocated(s%next_nod)) s%size_node = 0
            if(.not. allocated(s%list_nb_voxel_on)) s%nb_voxel_on = 0
              s%nb_voxel_on = 0
              call extend_array(s%last_nod, s%size_node,nsn+nsnr)
              call extend_array(s%next_nod, s%size_node ,nsn+nsnr)
              call extend_array(s%list_nb_voxel_on,s%size_node,nsn+nsnr)
              s%list_nb_voxel_on = 0
              s%size_node = max(s%size_node,nsn+nsnr)
              if(nsn + nsnr > 0) then
                s%last_nod(1:nsn+nsnr) = 0
                s%next_nod(1:nsn+nsnr) = 0
                s%list_nb_voxel_on(1:nsn+nsnr) = 0
              endif
            endif

              !nchunks is the number of groups              
              nchunks = (nsn + chunk - 1) / chunk
              !do ic = istart, nchunks ! for each chunk
               do while (flag == 0)
# ifdef MPI
!                  call MPI_Test(request, flag, MPI_STATUS_IGNORE, ierr)
                  if(nrequests > 0) then
                    call MPI_Testall(nrequests, requests, flag, MPI_STATUSES_IGNORE, ierr)
                  else
                    flag = 0 ! if no request: finish the job
!                   if((s%istart-1)*chunk+1 < nsn)write(6,*) 'start',(s%istart-1)*chunk+1,"nsn=",nsn,"nsnr=",nsnr

                  endif
#else
                flag = 0
#endif
                if(flag == 0 .and. s%istart <= nchunks) then
                  chunk_size = min(chunk, nsn - (s%istart-1)*chunk)
                  do k=1,chunk_size
                     i = (s%istart-1)*chunk + k
                     if(stfn(i) == zero)cycle
                     j=nsv(i)
                     if(x(1,j) < xmin)  cycle
                     if(x(1,j) > xmax)  cycle
                     if(x(2,j) < ymin)  cycle
                     if(x(2,j) > ymax)  cycle
                     if(x(3,j) < zmin)  cycle
                     if(x(3,j) > zmax)  cycle
                     ix=int(s%nbx*(x(1,j)-xminb)/(xmaxb-xminb))
                     iy=int(s%nby*(x(2,j)-yminb)/(ymaxb-yminb))
                     iz=int(s%nbz*(x(3,j)-zminb)/(zmaxb-zminb))
                     ix=max(1,2+min(s%nbx,ix))
                     iy=max(1,2+min(s%nby,iy))
                     iz=max(1,2+min(s%nbz,iz))
                     cellid = (iz-1)*(s%nbx+2)*(s%nby+2)+(iy-1)*(s%nbx+2)+ix
                     first = s%voxel(cellid)
                     if(first == 0)then
                       s%nb_voxel_on = s%nb_voxel_on + 1
                       s%list_nb_voxel_on(s%nb_voxel_on) = cellid
                       s%voxel(cellid) = i ! first
                       s%next_nod(i) = 0 ! last one
                       s%last_nod(i) = 0 ! no last
                     elseif(s%last_nod(first) == 0)then
                       s%next_nod(first) = i ! next
                       s%last_nod(first) = i ! last
                       s%next_nod(i)     = 0 ! last one
                     else
                       last = s%last_nod(first) ! last node in this voxel
                       s%next_nod(last)  = i ! next
                       s%last_nod(first) = i ! last
                       s%next_nod(i)     = 0 ! last one
                     endif
                  enddo !< k
                  s%istart = s%istart + 1
                else 
                  flag = 1
                endif !< flag
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
              call extend_array(list_nb_voxel_on, size_nod,nsn+nsnr)
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


                SUBROUTINE ASSOCIATE_VOXEL_MAIN(s,&
     &                                    irect        ,&
     &                                    x            ,&
     &                                    stf          ,&
     &                                    tzinf        ,&
     &                                    nrtm         ,&
     &                                    igap         ,&
     &                                    gap_m        ,&
     &                                    gapmin       ,&
     &                                    gapmax       ,&
     &                                    marge        ,&
     &                                    curv_max     ,&
     &                                    bgapsmx      ,&
     &                                    drad         ,&
     &                                    dgapload     ,&
     &                                    numnod       )
          USE CONSTANT_MOD
          USE INTER_STRUCT_MOD
!-----------------------------------------------
          implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

          TYPE(inter_struct_type), intent(inout) :: s !< structure for interface sorting comm
          integer, target :: nrtm !< number of segments
          integer, target :: igap !< gap model ?
          integer, target :: numnod !< total number of nodes of the model
          integer, target :: irect(4,nrtm) !< node id (from 1 to NUMNOD) for each segment (1:nrtm)
          my_real, target :: gapmin !< minimum gap
          my_real, target :: gapmax !< maximum gap
          my_real, target :: bgapsmx!< overestimation of gap_s
          my_real, target :: marge !< margin
          my_real, target :: tzinf !< some kind of length for "zone of influence" ?
          my_real, target :: drad !< radiation distance (thermal analysis)
          my_real, target :: dgapload !< gap load (???)
          my_real, target :: x(3,numnod) !< coordinates of nodes all
          my_real, target :: gap_m(nrtm) !< gap for main nodes
          my_real, target :: curv_max(nrtm) !< maximum curvature
          my_real, target :: stf(nrtm) !< stiffness of segments (quadrangles or triangles)

          s%ptr%nrtm => nrtm
          s%ptr%igap => igap
          !write(6,*) 'nrtm=',nrtm,'igap=',igap
          s%ptr%numnod => numnod
          s%ptr%irect => irect
          s%ptr%gapmin => gapmin
          s%ptr%gapmax => gapmax
          s%ptr%bgapsmx => bgapsmx
          s%ptr%marge => marge
          s%ptr%tzinf => tzinf
          s%ptr%drad => drad
          s%ptr%dgapload => dgapload
          s%ptr%x => x
          s%ptr%gap_m => gap_m
          s%ptr%curv_max => curv_max
          s%ptr%stf => stf
         
          end subroutine ASSOCIATE_VOXEL_MAIN


        SUBROUTINE FILL_VOXEL_MAIN(s)!,&
!    &                                    irect        ,&
!    &                                    x            ,&
!    &                                    stf          ,&
!    &                                    tzinf        ,&
!    &                                    nrtm         ,&
!    &                                    igap         ,&
!    &                                    gap_m        ,&
!    &                                    gapmin       ,&
!    &                                    gapmax       ,&
!    &                                    marge        ,&
!    &                                    curv_max     ,&
!    &                                    bgapsmx      ,&
!    &                                    drad         ,&
!    &                                    dgapload     ,&
!    &                                    numnod       )
          USE CONSTANT_MOD
          USE INTER_STRUCT_MOD
          USE MY_ALLOC_MOD, ONLY : my_alloc
          use extend_array_mod, only : extend_array


!-----------------------------------------------
          implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          TYPE(inter_struct_type), intent(inout) :: s !< structure for interface sorting comm
!         integer, intent(in) :: nrtm !< number of segments
!         integer, intent(in) :: igap !< gap model ?
!         integer, intent(in) :: numnod !< total number of nodes of the model
!         integer, intent(in) :: irect(4,nrtm) !< node id (from 1 to NUMNOD) for each segment (1:nrtm)
!         my_real, intent(in) :: gapmin !< minimum gap
!         my_real, intent(in) :: gapmax !< maximum gap
!         my_real, intent(in) :: bgapsmx!< overestimation of gap_s
!         my_real, intent(in) :: marge !< margin
!         my_real, intent(in) :: tzinf !< some kind of length for "zone of influence" ?
!         my_real, intent(in) :: drad !< radiation distance (thermal analysis)
!         my_real, intent(in) :: dgapload !< gap load (???)
!         my_real, intent(in) :: x(3,numnod) !< coordinates of nodes all
!         my_real, intent(in) :: gap_m(nrtm) !< gap for main nodes
!         my_real, intent(in) :: curv_max(nrtm) !< maximum curvature
!         my_real, intent(in) :: stf(nrtm) !< stiffness of segments (quadrangles or triangles)

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,j, nn, ne, k, l, jj, m
          my_real :: xs, ys, zs, sx, sy, sz, s2
          my_real :: xmin, xmax, ymin, ymax, zmin, zmax
          my_real :: xx1, xx2, xx3, xx4, yy1, yy2, yy3, yy4, zz1, zz2, zz3, zz4
          integer :: ix, iy, iz, m1, m2, m3, m4, ix1, iy1, iz1, ix2, iy2, iz2
          my_real :: xminb, yminb, zminb, xmaxb, ymaxb, zmaxb, xmine, ymine, zmine, xmaxe, ymaxe, zmaxe, aaa
          integer :: first, last
          integer :: cellid
          integer :: ll,llz

! Bounding box of the model
          xmin =s%box_limit_main(4)
          ymin =s%box_limit_main(5)
          zmin =s%box_limit_main(6)
          xmax =s%box_limit_main(1)
          ymax =s%box_limit_main(2)
          zmax =s%box_limit_main(3)

! reduced bounding box of the model
! The reduced bounding box corresponds to voxel(2:nbx+1,2:nby+1,2:nbz+1), it contains cells of the same size
          xminb =s%box_limit_main(10)
          yminb =s%box_limit_main(11)
          zminb =s%box_limit_main(12)
          xmaxb =s%box_limit_main(7)
          ymaxb =s%box_limit_main(8)
          zmaxb =s%box_limit_main(9)

!
!=======================================================================
! 3   FACE RECOVERY AND ENUMERATION OF CANDIDATE COUPLES
!=======================================================================
          do ne=1,s%ptr%nrtm
            if(s%ptr%stf(ne) == zero)cycle ! the segment is deleted/eroded
            if(s%ptr%igap == 0)then
              aaa = s%ptr%tzinf+s%ptr%curv_max(ne)
            else
              aaa = s%ptr%marge+s%ptr%curv_max(ne)+max(min(s%ptr%gapmax,max(s%ptr%gapmin,s%ptr%bgapsmx+&
              &s%ptr%gap_m(ne)))+s%ptr%dgapload,s%ptr%drad)
            endif
            m1 =s%ptr%irect(1,ne)
            m2 =s%ptr%irect(2,ne)
            m3 =s%ptr%irect(3,ne)
            m4 =s%ptr%irect(4,ne)
            xx1=s%ptr%x(1,m1)
            xx2=s%ptr%x(1,m2)
            xx3=s%ptr%x(1,m3)
            xx4=s%ptr%x(1,m4)
            xmaxe=max(xx1,xx2,xx3,xx4)
            xmine=min(xx1,xx2,xx3,xx4)
            yy1=s%ptr%x(2,m1)
            yy2=s%ptr%x(2,m2)
            yy3=s%ptr%x(2,m3)
            yy4=s%ptr%x(2,m4)
            ymaxe=max(yy1,yy2,yy3,yy4)
            ymine=min(yy1,yy2,yy3,yy4)
            zz1=s%ptr%x(3,m1)
            zz2=s%ptr%x(3,m2)
            zz3=s%ptr%x(3,m3)
            zz4=s%ptr%x(3,m4)
            zmaxe=max(zz1,zz2,zz3,zz4)
            zmine=min(zz1,zz2,zz3,zz4)

            ! surface (to trim candidate list)
            sx = (yy3-yy1)*(zz4-zz2) - (zz3-zz1)*(yy4-yy2)
            sy = (zz3-zz1)*(xx4-xx2) - (xx3-xx1)*(zz4-zz2)
            sz = (xx3-xx1)*(yy4-yy2) - (yy3-yy1)*(xx4-xx2)
            s2 = sx*sx + sy*sy + sz*sz

            !find voxel_mains containing the bounding box of the segment
            if(s%nbx>1) then
              ix1=int(s%nbx*(xmine-aaa-xminb)/(xmaxb-xminb))
              ix2=int(s%nbx*(xmaxe+aaa-xminb)/(xmaxb-xminb))
            else
              ix1=-2
              ix2=1
            endif

            if(s%nby>1) then
              iy1=int(s%nby*(ymine-aaa-yminb)/(ymaxb-yminb))
              iy2=int(s%nby*(ymaxe+aaa-yminb)/(ymaxb-yminb))
            else
              iy1=-2
              iy2=1
            endif

            if(s%nbz>1) then
              iz1=int(s%nbz*(zmine-aaa-zminb)/(zmaxb-zminb))
              iz2=int(s%nbz*(zmaxe+aaa-zminb)/(zmaxb-zminb))
            else
              iz1=-2
              iz2=1
            endif

            ix1=max(1,2+min(s%nbx,ix1))
            iy1=max(1,2+min(s%nby,iy1))
            iz1=max(1,2+min(s%nbz,iz1))

            ix2=max(1,2+min(s%nbx,ix2))
            iy2=max(1,2+min(s%nby,iy2))
            iz2=max(1,2+min(s%nbz,iz2))
            
            ! im1 = id secondary or -1 
            ! im1 = inv_nsv(m1)
            ! im2 = inv_nsv(m2)
            ! im3 = inv_nsv(m3)
            ! im4 = inv_nsv(m4)
            do iz = iz1, iz2
              do iy = iy1, iy2
                do ix = ix1, ix2
 
                cellid = (iz-1)*(s%nbx+2)*(s%nby+2)+(iy-1)*(s%nbx+2)+ix
                ! append(s%main( compact_cellid(cellid) )%list,ne))
                first = s%voxel_main(cellid)%nb + 1
                if(.NOT. allocated(s%voxel_main(cellid)%list)) then
                  call my_alloc(s%voxel_main(cellid)%list, 4)
                end if
                if(first > size(s%voxel_main(cellid)%list)) then
                  call extend_array(s%voxel_main(cellid)%list, s%voxel_main(cellid)%nb , first*2)
                endif
                s%voxel_main(cellid)%nb = s%voxel_main(cellid)%nb + 1
                s%voxel_main(cellid)%list(first) = ne
 
                enddo ! x
              enddo  ! y
            enddo   ! z

          enddo
          return
        end



      END MODULE FILL_VOXEL_MOD
