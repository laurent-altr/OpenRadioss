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
!||    inter7_candidate_pairs_mod   ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
!||--- called by ------------------------------------------------------
!||    inter7_collision_detection   ../engine/source/interfaces/intsort/inter7_collision_detection.F90
!||    main                         ../engine/unit_test/unit_test1.F
!||====================================================================
      MODULE INTER7_CANDIDATE_PAIRS_MOD
        implicit none
      CONTAINS

!! \brief get the list of candidates for all segment
!hd|====================================================================
!hd|  INTER7_CANDIDATE_PAIRS        source/interfaces/intsort/inter7_candidate_pairs.F
!hd|-- called by -----------
!hd|        INTER7_COLLISION_DETECTION    source/interfaces/inter7_collision_detection.F
!hd|-- calls ---------------
!hd|        COLLISION_MOD                 source/interfaces/intsort/collision_mod.F
!hd|        INTER7_FILTER_CAND_MOD        source/interfaces/intsort/inter7_filter_cand.F
!hd|====================================================================
        SUBROUTINE INTER7_CANDIDATE_PAIRS(&
        &nsn          ,& !1
        &oldnum       ,& !2
        &nsnr         ,& !3
        &isznsnr      ,& !4
        &i_mem        ,& !5
        &irect        ,& !6
        &x            ,& !7
        &stf          ,&
        &stfn         ,&
        &xyzm         ,&
        &nsv          ,&
        &ii_stok      ,&
        &cand_n       ,&
        &eshift       ,&
        &cand_e       ,&
        &mulnsn       ,&
        &tzinf        ,&
        &gap_s_l      ,&
        &gap_m_l      ,&
        &voxel        ,&
        &nbx          ,&
        &nby          ,&
        &nbz          ,&
        &inacti       ,&
        &ifq          ,&
        &cand_a       ,&
        &cand_p       ,&
        &ifpen        ,&
        &nrtm         ,&
        &nsnrold      ,&
        &igap         ,&
        &gap          ,&
        &gap_s        ,&
        &gap_m        ,&
        &gapmin       ,&
        &gapmax       ,&
        &marge        ,&
        &curv_max     ,&
        &itask        ,&
        &bgapsmx      ,&
        &s_kremnod    ,&
        &kremnod      ,&
        &s_remnod     ,&
        &remnod       ,&
        &flagremnode  ,&
        &drad         ,&
        &itied        ,&
        &cand_f       ,&
        &dgapload     ,&
        &s_cand_a     ,&
        &total_nb_nrtm,&
        &numnod       ,&
        &xrem         ,&
        &s_xrem       ,&
        &irem         ,&
        &s_irem       )
          USE INTER7_FILTER_CAND_MOD
          USE CONSTANT_MOD
          use gpu_pref
          use precision_mod, only : WP
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(inout) :: i_mem !< error code when not enough memory
          integer, intent(in), value :: eshift !< openmp shift for main segments
          integer, intent(in), value :: nsn !< number of secondary nodes
          integer, intent(in), value :: nsnr !< current number of remote secondary nodes
          integer, intent(in), value :: nsnrold !< old number of remote secondary nodes
          integer, intent(in), value :: isznsnr !< size of oldnum
          integer, intent(in), value :: nrtm !< number of considered segment
          integer, intent(in), value :: total_nb_nrtm !< total number of segments
          integer, intent(in), value :: itask !< id of the current task
          integer, intent(in), value :: nbx !< number of voxels in x
          integer, intent(in), value :: nby !< number of voxels in y
          integer, intent(in), value :: nbz !< number of voxels in z
          integer, intent(in), value :: inacti !< inactivation of initial penetrations
          integer, intent(in), value :: ifq !< friction model ?
          integer, intent(in), value :: igap !< gap model ?
          integer, intent(in), value :: flagremnode !< flag for removed nodes?
          integer, intent(in), value :: itied !< tied contact ?
          integer, intent(in), value :: numnod !< total number of nodes of the model
          integer, intent(in), value :: s_xrem !< size of xrem
          integer, intent(in), value :: s_irem !< size of xrem
          integer, intent(in), value :: s_cand_a !< size of cand_a
          integer, intent(in), value :: s_kremnod !< 2 * nrtm + 1 if option is used
          integer, intent(in), value :: s_remnod !< size of remnod
          integer, intent(in), value :: mulnsn !< maximum numbrer of candidates (size of cand_n)
          integer, intent(inout) :: ii_stok !< number of candidates found

          integer, intent(in) :: nsv(nsn) !< global secondary node numbers
          integer, intent(in) :: oldnum(isznsnr) !< renumbering ?
          integer, intent(in) :: kremnod(s_kremnod) !< list of removed nodes
          integer, intent(in) :: remnod(s_remnod) !< list of removed nodes
          integer, intent(in) :: irect(4,nrtm) !< node id (from 1 to NUMNOD) for each segment (1:nrtm)

          integer, intent(inout) :: cand_n(mulnsn) !< list of candidates (secondary)
          integer, intent(inout) :: cand_e(mulnsn) !< list of candidates (main)
          integer, intent(inout) :: ifpen(mulnsn) !< something related to friction (???)
          integer, intent(inout) :: cand_a(s_cand_a) !< (???)
          integer, intent(inout) :: irem(s_irem,nsnr) !< remote (spmd) integer data
          integer, intent(inout) :: voxel(nbx+2,nby+2,nbz+2) !< contain the first node of each voxel
!            integer, intent(inout) :: next_nod(nsn+nsnr) !< next node in the same voxel

          real(WP), intent(in), value :: gap !< gap (???)
          real(WP), intent(in), value :: gapmin !< minimum gap
          real(WP), intent(in), value :: gapmax !< maximum gap
          real(WP), intent(in), value :: bgapsmx!< overestimation of gap_s
          real(WP), intent(in), value :: marge !< margin
          real(WP), intent(in), value :: tzinf !< some kind of length for "zone of influence" ?
          real(WP), intent(in), value :: drad !< radiation distance (thermal analysis)
          real(WP), intent(in), value :: dgapload !< gap load (???)
          real(WP), intent(in) :: x(3,numnod) !< coordinates of nodes all
          real(WP), intent(in) :: gap_s(nsn) !< gap for secondary nodes
          real(WP), intent(in) :: gap_m(nrtm) !< gap for main nodes
          real(WP), intent(in) :: gap_s_l(nsn) !< gap for secondary nodes (???)
          real(WP), intent(in) :: gap_m_l(nrtm) !< gap for main nodes (???)
          real(WP), intent(in) :: curv_max(nrtm) !< maximum curvature
          real(WP), intent(in) :: xyzm(12) !< bounding box

          real(WP), intent(inout) :: cand_p(mulnsn) !< penetration (???)
          real(WP), intent(inout) :: cand_f(mulnsn) !< related to tied contact, cand force (???)
          real(WP), intent(in) :: stf(nrtm) !< stiffness of segments (quadrangles or triangles)
          real(WP), intent(inout) :: stfn(nsn) !< stiffness secondary nodes
          real(WP), intent(inout) :: xrem(s_xrem,nsnr) !< remote (spmd) real data
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer i,j,&
          &nn,ne,k,l,j_stok,jj,&
          &delnod,m,j_stok_local
          integer, dimension(:), allocatable :: tagremnode
          real(WP)&
          &xs,ys,zs,sx,sy,sz,s2,&
          &xmin, xmax,ymin, ymax,zmin, zmax,&
          &xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,&
          &d1x,d1y,d1z,d2x,d2y,d2z,dd1,dd2,d2,a2
          integer  ix,iy,iz,m1,m2,m3,m4,&
          &ix1,iy1,iz1,ix2,iy2,iz2
          integer :: iix,iiy,iiz
          real(WP)&
          &xminb,yminb,zminb,xmaxb,ymaxb,zmaxb,&
          &xmine,ymine,zmine,xmaxe,ymaxe,zmaxe,aaa
          integer first,last,idx,idx2
          integer, dimension(:), allocatable :: prov_n, prov_e, j_stok_arr !< temporary list of candidates
          integer, dimension(:), allocatable :: iix_arr,iiy_arr,iiz_arr,skip_arr
          integer :: n_pot_cand
          ! new data structures
          integer, dimension(:,:), allocatable :: nnz_voxel !< compressed matrix, rows are voxels, column are nodes
          integer, dimension(:), allocatable :: row_ptr !< row_ptr for nnz_voxel
          integer, dimension(:), allocatable :: voxel_to_nnzvoxel !< translation array from orig_voxel_id to compresed_voxel_id
          integer, dimension(:), allocatable :: nnodes_on_voxel !< number of nodes on each voxel
          integer :: max_nodes_on_voxel,number_nnz_voxels,row_ptr_tmp

          allocate(j_stok_arr(1))
          allocate(skip_arr(nsn+nsnr))
          allocate(iix_arr(nsn+nsnr))
          allocate(iiy_arr(nsn+nsnr))
          allocate(iiz_arr(nsn+nsnr))
          ! new data structures
          allocate(voxel_to_nnzvoxel((nbx+2)*(nby+2)*(nbz+2)))
          allocate(nnodes_on_voxel((nbx+2)*(nby+2)*(nbz+2)))

          n_pot_cand = 0
          number_nnz_voxels = 0

          !call set_prefloc(j_stok_arr)
          ! new data structures
          !call set_prefloc(voxel_to_nnzvoxel)
          !call set_prefloc(nnodes_on_voxel)

          ! Allocate tagremnode before DATA regions so it can be placed on the device
          if(flagremnode == 2) then
            allocate(tagremnode(numnod))
          else
            allocate(tagremnode(1))
          endif
          !call set_prefloc(tagremnode)
          tagremnode = 0

          !Vars for INTER7_FILTER_CAND

!-----------------------------------------------

!$OMP BARRIER

! The global bounding box contains all the nodes
! Some nodes may by higly distant from the impact zone
! The domain is subdivided in cells (voxel)
! All the cells have the sime size, except the first and the last one in each direction

! bounding box of the model
          xmin = xyzm(1)
          ymin = xyzm(2)
          zmin = xyzm(3)
          xmax = xyzm(4)
          ymax = xyzm(5)
          zmax = xyzm(6)

! reduced bounding box of the model
! The reduced bounding box corresponds to voxel(2:nbx+1,2:nby+1,2:nbz+1), it contains cells of the same size

          xminb = xyzm(7)
          yminb = xyzm(8)
          zminb = xyzm(9)
          xmaxb = xyzm(10)
          ymaxb = xyzm(11)
          zmaxb = xyzm(12)

!$ACC DATA CREATE(voxel) COPYIN(nsv,stfn,x,kremnod,stf,irect,curv_max,gap_m,gap_s,gap_m_l,gap_s_l,remnod,irem)
!$ACC DATA CREATE(CAPTURE:skip_arr,iix_arr,iiy_arr,iiz_arr) COPYIN(xrem)
!$ACC DATA CREATE(CAPTURE:voxel_to_nnzvoxel,nnodes_on_voxel,tagremnode,j_stok_arr) COPY(number_nnz_voxels)

!=======================================================================
! 1+2   Add local and remote nodes to the cells
!=======================================================================
          ! Phases:
          !  1. compute number of nodes per each voxel
          !  2. create translation of voxel_id to nnz_voxel_id
          !  3. Find max_nodes_pero_voxel (i.e., 2nd dimension for nnz_voxel matrix)
          !  4. create and populate nnz_voxel now that we know the maximum possible 2nd dimension
          ! The IDEA here is to find how big a compressed matrix where i-th row is a non-empty voxel and
          ! each column contains the j-th node of the i-th non-empty voxel.
          ! As a side node, we process both local and remote nodes at the same time now
          call nvtxStartRange("add_nodes_to_cells", 10)
!-------------------------------------------------------------------------------------------------------
          ! 1. compute number of nodes per each voxel
          call nvtxStartRange("add_nodes_to_cells_phase1", 101)
!$ACC KERNELS DEFAULT(PRESENT)
          nnodes_on_voxel = 0
!$ACC END KERNELS
!$ACC PARALLEL LOOP VECTOR GANG DEFAULT(PRESENT)
          do i=1,nsn+nsnr
            ! For each node, just find the containing voxel and increment the corresponding
            ! counter in nnodes_on_voxel array
            ! We also store iix,iiy,iiz of each node for future phases in add_nodes_to_cells
            ! We also store if the current node was skipped so we can skip in future phases
            skip_arr(i) = 1
            if (i <= nsn) then
              ! local node
              if(stfn(i) == zero)cycle
              j=nsv(i)
              if(x(1,j) < xmin)  cycle
              if(x(1,j) > xmax)  cycle
              if(x(2,j) < ymin)  cycle
              if(x(2,j) > ymax)  cycle
              if(x(3,j) < zmin)  cycle
              if(x(3,j) > zmax)  cycle

              iix=int(nbx*(x(1,j)-xminb)/(xmaxb-xminb))
              iiy=int(nby*(x(2,j)-yminb)/(ymaxb-yminb))
              iiz=int(nbz*(x(3,j)-zminb)/(zmaxb-zminb))
            else
              j=i-nsn
              ! remote node
              if(xrem(1,j) < xmin)  cycle
              if(xrem(1,j) > xmax)  cycle
              if(xrem(2,j) < ymin)  cycle
              if(xrem(2,j) > ymax)  cycle
              if(xrem(3,j) < zmin)  cycle
              if(xrem(3,j) > zmax)  cycle

              iix=int(nbx*(xrem(1,j)-xminb)/(xmaxb-xminb))
              iiy=int(nby*(xrem(2,j)-yminb)/(ymaxb-yminb))
              iiz=int(nbz*(xrem(3,j)-zminb)/(zmaxb-zminb))
            endif
            iix=max(1,2+min(nbx,iix))
            iiy=max(1,2+min(nby,iiy))
            iiz=max(1,2+min(nbz,iiz))
            skip_arr(i) = 0
            iix_arr(i) = iix
            iiy_arr(i) = iiy
            iiz_arr(i) = iiz
            idx = (iix-1)*(nby+2)*(nbz+2)+(iiy-1)*(nbz+2)+iiz

!$ACC ATOMIC UPDATE
            nnodes_on_voxel(idx) = nnodes_on_voxel(idx) + 1

          enddo
          call nvtxEndRange()
!-------------------------------------------------------------------------------------------------------
          ! 2. create translation of voxel id to nnz_voxel_id
          ! 3. Find max_nodes_pero_voxel (i.e., 2nd dimension for nnz_voxel matrix)
          call nvtxStartRange("add_nodes_to_cells_phase2", 102)
          max_nodes_on_voxel = 0
!$ACC PARALLEL LOOP GANG VECTOR DEFAULT(PRESENT) REDUCTION(max:max_nodes_on_voxel)
          do i=1,(nbx+2)*(nby+2)*(nbz+2)
            ! we traverse over voxels this time since we don't want to repeat them
            ! this can be done because we know how many nodes we have in each voxel from previous step
            if ( nnodes_on_voxel(i) > 0 ) then
              ! this voxel is non-empty, se we increment the number of
              ! non-empty voxels by 1, then we assign this voxel
              ! the next available row on the compressed matrix
!$ACC ATOMIC CAPTURE
              number_nnz_voxels = number_nnz_voxels + 1
              voxel_to_nnzvoxel(i) = number_nnz_voxels
!$ACC END ATOMIC
              max_nodes_on_voxel = max(max_nodes_on_voxel,nnodes_on_voxel(i))
            endif
          enddo
          call nvtxEndRange()
!$ACC UPDATE SELF(number_nnz_voxels)
!-------------------------------------------------------------------------------------------------------
          ! 4. create and populate nnz_voxel now that we know the maximum possible 2nd dimension
          call nvtxStartRange("add_nodes_to_cells_phase3", 103)
          allocate(nnz_voxel(number_nnz_voxels,max_nodes_on_voxel))
          allocate(row_ptr(number_nnz_voxels))
          !call set_prefloc(nnz_voxel)
          !call set_prefloc(row_ptr)

!$ACC DATA CREATE(CAPTURE:nnz_voxel,row_ptr)

!$ACC KERNELS DEFAULT(PRESENT)
          row_ptr = 0
          nnz_voxel = 0
!$ACC END KERNELS

!$ACC PARALLEL LOOP GANG VECTOR DEFAULT(PRESENT)
          do i=1,nsn+nsnr
            ! for each non-skipped node, find containing voxel and store in nnz_voxel
            if ( skip_arr(i) == 1 ) cycle
            iix = iix_arr(i)
            iiy = iiy_arr(i)
            iiz = iiz_arr(i)
            idx = (iix-1)*(nby+2)*(nbz+2)+(iiy-1)*(nbz+2)+iiz
!$ACC ATOMIC CAPTURE
            row_ptr(voxel_to_nnzvoxel(idx)) = row_ptr(voxel_to_nnzvoxel(idx)) + 1
            row_ptr_tmp=row_ptr(voxel_to_nnzvoxel(idx))
!$ACC END ATOMIC
            nnz_voxel(voxel_to_nnzvoxel(idx),row_ptr_tmp) = i
          enddo
          call nvtxEndRange()
          call nvtxEndRange()

!$OMP BARRIER

!=======================================================================
! 3   FACE RECOVERY AND ENUMERATION OF CANDIDATE COUPLES
!=======================================================================
          call nvtxStartRange("face_recovery_enum_candidate_couples",12)
!$ACC KERNELS DEFAULT(PRESENT)
          j_stok_arr(1)=0
          tagremnode = 0
!$ACC END KERNELS
          j_stok = 0
!$OMP BARRIER

!=======================================================================
! 3.1   FIND HOW MANY CANDIDATES CAN YOU POTENTIALLY HAVE
!=======================================================================
!$ACC PARALLEL LOOP GANG VECTOR DEFAULT(PRESENT) REDUCTION(+:n_pot_cand)
          do ne=1,nrtm
            if(stf(ne) == zero)cycle ! the segment is deleted/eroded
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                ! the segment ne cannot be in contact with the node remnod(i)
                ! typically, remnod(i) contains nodes of neighboring elements
                tagremnode(remnod(i)) = 1
              enddo
            endif
            if(igap == 0)then
              aaa = tzinf+curv_max(ne)
            else
              aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,bgapsmx+gap_m(ne)))+dgapload,drad)
            endif

            m1 = irect(1,ne)
            m2 = irect(2,ne)
            m3 = irect(3,ne)
            m4 = irect(4,ne)

            xx1=x(1,m1)
            xx2=x(1,m2)
            xx3=x(1,m3)
            xx4=x(1,m4)
            xmaxe=max(xx1,xx2,xx3,xx4)
            xmine=min(xx1,xx2,xx3,xx4)

            yy1=x(2,m1)
            yy2=x(2,m2)
            yy3=x(2,m3)
            yy4=x(2,m4)
            ymaxe=max(yy1,yy2,yy3,yy4)
            ymine=min(yy1,yy2,yy3,yy4)

            zz1=x(3,m1)
            zz2=x(3,m2)
            zz3=x(3,m3)
            zz4=x(3,m4)
            zmaxe=max(zz1,zz2,zz3,zz4)
            zmine=min(zz1,zz2,zz3,zz4)

            ! surface (to trim candidate list)
            sx = (yy3-yy1)*(zz4-zz2) - (zz3-zz1)*(yy4-yy2)
            sy = (zz3-zz1)*(xx4-xx2) - (xx3-xx1)*(zz4-zz2)
            sz = (xx3-xx1)*(yy4-yy2) - (yy3-yy1)*(xx4-xx2)
            s2 = sx*sx + sy*sy + sz*sz

            !find voxels containing the bounding box of the segment
            if(nbx>1) then
              ix1=int(nbx*(xmine-aaa-xminb)/(xmaxb-xminb))
              ix2=int(nbx*(xmaxe+aaa-xminb)/(xmaxb-xminb))
            else
              ix1=-2
              ix2=1
            endif

            if(nby>1) then
              iy1=int(nby*(ymine-aaa-yminb)/(ymaxb-yminb))
              iy2=int(nby*(ymaxe+aaa-yminb)/(ymaxb-yminb))
            else
              iy1=-2
              iy2=1
            endif

            if(nbz>1) then
              iz1=int(nbz*(zmine-aaa-zminb)/(zmaxb-zminb))
              iz2=int(nbz*(zmaxe+aaa-zminb)/(zmaxb-zminb))
            else
              iz1=-2
              iz2=1
            endif

            ix1=max(1,2+min(nbx,ix1))
            iy1=max(1,2+min(nby,iy1))
            iz1=max(1,2+min(nbz,iz1))

            ix2=max(1,2+min(nbx,ix2))
            iy2=max(1,2+min(nby,iy2))
            iz2=max(1,2+min(nbz,iz2))

            do iz = iz1,iz2
              do iy = iy1,iy2
                do ix = ix1,ix2
                  idx = (ix-1)*(nby+2)*(nbz+2)+(iy-1)*(nbz+2)+iz
                  n_pot_cand = n_pot_cand + nnodes_on_voxel(idx)
                end do
              end do
            end do
          end do

          allocate(prov_n(n_pot_cand))
          allocate(prov_e(n_pot_cand))
          if(mulnsn < n_pot_cand) then
            i_mem = 2
          endif
          !call set_prefloc(prov_n)
          !call set_prefloc(prov_e)

!$ACC DATA CREATE(CAPTURE:prov_n,prov_e)

!$ACC PARALLEL LOOP GANG VECTOR DEFAULT(PRESENT)
          do ne=1,nrtm
            if(stf(ne) == zero)cycle ! the segment is deleted/eroded
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                ! the segment ne cannot be in contact with the node remnod(i)
                ! typically, remnod(i) contains nodes of neighboring elements
!$ACC ATOMIC UPDATE
                tagremnode(remnod(i)) = 1
              enddo
            endif
            if(igap == 0)then
              aaa = tzinf+curv_max(ne)
            else
              aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,bgapsmx+gap_m(ne)))+dgapload,drad)
            endif


            m1 = irect(1,ne)
            m2 = irect(2,ne)
            m3 = irect(3,ne)
            m4 = irect(4,ne)

            xx1=x(1,m1)
            xx2=x(1,m2)
            xx3=x(1,m3)
            xx4=x(1,m4)
            xmaxe=max(xx1,xx2,xx3,xx4)
            xmine=min(xx1,xx2,xx3,xx4)

            yy1=x(2,m1)
            yy2=x(2,m2)
            yy3=x(2,m3)
            yy4=x(2,m4)
            ymaxe=max(yy1,yy2,yy3,yy4)
            ymine=min(yy1,yy2,yy3,yy4)

            zz1=x(3,m1)
            zz2=x(3,m2)
            zz3=x(3,m3)
            zz4=x(3,m4)
            zmaxe=max(zz1,zz2,zz3,zz4)
            zmine=min(zz1,zz2,zz3,zz4)

            ! surface (to trim candidate list)
            sx = (yy3-yy1)*(zz4-zz2) - (zz3-zz1)*(yy4-yy2)
            sy = (zz3-zz1)*(xx4-xx2) - (xx3-xx1)*(zz4-zz2)
            sz = (xx3-xx1)*(yy4-yy2) - (yy3-yy1)*(xx4-xx2)
            s2 = sx*sx + sy*sy + sz*sz

            !find voxels containing the bounding box of the segment
            if(nbx>1) then
              ix1=int(nbx*(xmine-aaa-xminb)/(xmaxb-xminb))
              ix2=int(nbx*(xmaxe+aaa-xminb)/(xmaxb-xminb))
            else
              ix1=-2
              ix2=1
            endif

            if(nby>1) then
              iy1=int(nby*(ymine-aaa-yminb)/(ymaxb-yminb))
              iy2=int(nby*(ymaxe+aaa-yminb)/(ymaxb-yminb))
            else
              iy1=-2
              iy2=1
            endif

            if(nbz>1) then
              iz1=int(nbz*(zmine-aaa-zminb)/(zmaxb-zminb))
              iz2=int(nbz*(zmaxe+aaa-zminb)/(zmaxb-zminb))
            else
              iz1=-2
              iz2=1
            endif

            ix1=max(1,2+min(nbx,ix1))
            iy1=max(1,2+min(nby,iy1))
            iz1=max(1,2+min(nbz,iz1))

            ix2=max(1,2+min(nbx,ix2))
            iy2=max(1,2+min(nby,iy2))
            iz2=max(1,2+min(nbz,iz2))

            do iz = iz1,iz2
              do iy = iy1,iy2
                do ix = ix1,ix2
                  idx = (ix-1)*(nbz+2)*(nby+2) + (iy-1)*(nbz+2) + iz
                  do idx2=1,nnodes_on_voxel(idx)
                    jj = nnz_voxel(voxel_to_nnzvoxel(idx),idx2)

                    if(jj<=nsn)then
                      ! local node
                      nn=nsv(jj)

                      if(nn == m1)goto 200
                      if(nn == m2)goto 200
                      if(nn == m3)goto 200
                      if(nn == m4)goto 200

                      if(flagremnode == 2) then
                        if( tagremnode(nsv(jj)) == 1) goto 200
                      endif
                      xs = x(1,nn)
                      ys = x(2,nn)
                      zs = x(3,nn)
                      if(igap /= 0)then
                        aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,gap_s(jj)+gap_m(ne)))+dgapload,drad)
                      endif
                    else
                      ! remote (SPMD) node: data are stored in irem/xrem (communicated earlier)
                      j=jj-nsn
                      delnod = 0
                      if(flagremnode == 2) then
                        k = kremnod(2*(ne-1)+2) + 1
                        l = kremnod(2*(ne-1)+3)
                        do m=k,l
                          if(remnod(m) == -irem(2,j) ) then
                            delnod = delnod + 1
                            exit
                          endif
                        enddo
                        if(delnod /= 0)goto 200
                      endif

                      xs = xrem(1,j)
                      ys = xrem(2,j)
                      zs = xrem(3,j)
                      if(igap /= 0)then
                        aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,xrem(9,j)+gap_m(ne)))+dgapload,drad)
                      endif
                    endif

                    if(xs<=xmine-aaa)goto 200
                    if(xs>=xmaxe+aaa)goto 200
                    if(ys<=ymine-aaa)goto 200
                    if(ys>=ymaxe+aaa)goto 200
                    if(zs<=zmine-aaa)goto 200
                    if(zs>=zmaxe+aaa)goto 200

                    ! underestimation of the distance**2 to eliminate candidates

                    d1x = xs - xx1
                    d1y = ys - yy1
                    d1z = zs - zz1
                    d2x = xs - xx2
                    d2y = ys - yy2
                    d2z = zs - zz2
                    dd1 = d1x*sx+d1y*sy+d1z*sz
                    dd2 = d2x*sx+d2y*sy+d2z*sz
                    if(dd1*dd2 > zero)then
                      d2 = min(dd1*dd1,dd2*dd2)
                      a2 = aaa*aaa*s2
                      if(d2 > a2)goto 200
                    endif

!$ACC ATOMIC CAPTURE
                    j_stok_arr(1)=j_stok_arr(1)+1
                    j_stok_local = j_stok_arr(1)
!$ACC END ATOMIC

                    prov_n(j_stok_local) = jj
                    prov_e(j_stok_local) = ne
200                 continue
!                     if(i_mem==2) jj = 0
                  enddo ! while(jj /= 0)
                enddo ! x
              enddo  ! y
            enddo   ! z
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                tagremnode(remnod(i)) = 0
              enddo
            endif
          enddo

          call nvtxEndRange()
          call nvtxStartRange("inter7_filter_cand")
!$ACC UPDATE SELF(j_stok_arr)
          j_stok=j_stok_arr(1)
          if(j_stok > 0 .and. i_mem == 0) then

            call inter7_filter_cand(&
            &j_stok,irect  ,x     ,nsv   ,ii_stok,&
            &cand_n,cand_e ,mulnsn,marge  ,&
            &i_mem ,prov_n ,prov_e,eshift,inacti ,&
            &ifq   ,cand_a ,cand_p,ifpen ,nsn    ,&
            &oldnum,nsnrold,igap  ,gap   ,gap_s  ,&
            &gap_m ,gapmin ,gapmax,curv_max,&
            &gap_s_l,gap_m_l,drad,itied    ,&
            &cand_f ,dgapload,&
            &nsnr,&
            &xrem ,s_xrem )
          endif
          call nvtxEndRange()
!$ACC END DATA
!$ACC END DATA
!$ACC END DATA
!$ACC END DATA
!$ACC END DATA
!=======================================================================
! 5   VOXEL RESET
!=======================================================================
          call nvtxStartRange("voxel_reset", 13)
!$OMP BARRIER
          if(total_nb_nrtm>0 .and. itask == 0) then
            voxel = 0
          endif
!$OMP BARRIER
          call nvtxEndRange()
!=======================================================================
! 7   DEALLOCATE
!=======================================================================
          call nvtxStartRange("deallocate", 14)
          if(allocated(tagremnode)) deallocate(tagremnode)

          call nvtxEndRange()

          return
        end




      end module INTER7_CANDIDATE_PAIRS_MOD



