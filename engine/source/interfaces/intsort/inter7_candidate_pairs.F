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
      MODULE INTER7_CANDIDATE_PAIRS_MOD
        integer :: max_node_per_cell !< maximum number of nodes in a cell
      CONTAINS
Chd|====================================================================
Chd|  I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|-- called by -----------
Chd|        I7BUCE_VOX                    source/interfaces/intsort/i7buce.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F
Chd|        I7STO                         source/interfaces/intsort/i7sto.F
Chd|        MY_BARRIER                    source/system/machine.F
Chd|        SPMD_OLDNUMCD                 source/mpi/interfaces/spmd_i7tool.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F
Chd|====================================================================
        SUBROUTINE INTER7_CANDIDATE_PAIRS(
     1        nsn    ,oldnum ,nsnr    ,isznsnr  ,i_mem    ,
     2        irect  ,x           ,stf     ,stfn     ,xyzm     ,
     3        nsv    ,ii_stok     ,cand_n  ,eshift   ,cand_e   ,
     4        mulnsn ,tzinf   ,gap_s_l  ,gap_m_l  ,
     5        voxel  ,nbx         ,nby     ,nbz      ,
     6        inacti ,ifq         ,cand_a  ,cand_p   ,ifpen    ,
     7        nrtm   ,nsnrold     ,igap    ,gap      ,gap_s    ,
     8        gap_m  ,gapmin      ,gapmax  ,marge    ,curv_max ,
     9        itask       ,bgapsmx ,kremnod  ,remnod   ,
     a        flagremnode ,drad    ,itied    ,cand_f   ,
     b        dgapload, s_cand_a,
     c        total_nb_nrtm,  numnod, xrem     ,s_xrem,
     e        irem, s_irem, next_nod)

          USE INTER7_FILTER_CAND_MOD
          USE CONSTANT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
          implicit none
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          integer, intent(inout) :: i_mem !< error code when not enough memory
          integer, intent(in) :: eshift !< openmp shift for main segments
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
          integer, intent(in) :: nsv(nsn) !< global secondary node numbers
          integer, intent(in) :: oldnum(isznsnr) !< renumbering ?
          integer, intent(in) :: kremnod(*) !< list of removed nodes
          integer, intent(in) :: remnod(*) !< list of removed nodes
          integer, intent(in) :: irect(4,nrtm) !< node id (from 1 to NUMNOD) for each segment (1:nrtm)
          integer, intent(inout) :: ii_stok !< number of candidates found
          integer, intent(inout) :: mulnsn !< maximum numbrer of candidates (size of cand_n)
          integer, intent(inout) :: cand_n(mulnsn) !< list of candidates (secodnary)
          integer, intent(inout) :: cand_e(mulnsn) !< list of candidates (main)
          integer, intent(inout) :: ifpen(mulnsn) !< something related to friction??
          integer, intent(inout) :: cand_a(s_cand_a) !< ??
          integer, intent(inout) :: irem(s_irem,nsnr) !< remote (spmd) integer data
          integer, intent(inout) :: voxel(nbx+2,nby+2,nbz+2) !< contain the first node of each voxel
          integer, intent(inout) :: next_nod(nsn+nsnr) !< next node in the same voxel
          my_real, intent(in) :: x(3,numnod) !< coordinates of nodes all
          my_real, intent(inout) :: cand_p(mulnsn) !< penetration ??
          my_real, intent(in), value :: gap !< gap for
          my_real, intent(in) :: gap_s(nsn) !< gap for secondary nodes
          my_real, intent(in) :: gap_m(nrtm) !< gap for main nodes
          my_real, intent(in) :: gap_s_l(nsn) !< gap for secondary nodes (???)
          my_real, intent(in) :: gap_m_l(nrtm) !< gap for main nodes (???)
          my_real, intent(in), value :: gapmin !< minimum gap
          my_real, intent(in), value :: gapmax !< maximum gap
          my_real, intent(in), value :: bgapsmx!< overestimation of gap_s
          my_real, intent(in), value :: marge !< margin
          my_real, intent(in), value :: tzinf !< some kind of length for "zone of influence" ?
          my_real, intent(in) :: curv_max(nrtm) !< maximum curvature
          my_real, intent(in) :: xyzm(12) !< bounding box
          my_real, intent(in), value :: drad !< radiation distance (thermal analysis)
          my_real, intent(in), value :: dgapload !< gap load (???)
          my_real, intent(inout) :: cand_f(mulnsn) !< related to tied contact, cand force???
          my_real, intent(inout) :: stf(nrtm) !< stiffness of segments
          my_real, intent(inout) :: stfn(nsn) !< stiffness secondary nodes
          my_real, intent(inout) :: xrem(s_xrem,nsnr) !< remote (spmd) real data
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          integer i,j,
     .            nn,ne,k,l,j_stok,ii,jj,
     .            nsnf, nsnl,delnod,m
          integer, dimension(:), allocatable :: tagremnode
          my_real
     .       xs,ys,zs,xx,sx,sy,sz,s2,
     .       xmin, xmax,ymin, ymax,zmin, zmax,
     .       xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     .       d1x,d1y,d1z,d2x,d2y,d2z,dd1,dd2,d2,a2
          integer, dimension(:), allocatable::  last_nod
          integer  ix,iy,iz,m1,m2,m3,m4,
     .             ix1,iy1,iz1,ix2,iy2,iz2
          integer :: iix,iiy,iiz
          my_real
     .       xminb,yminb,zminb,xmaxb,ymaxb,zmaxb,
     .       xmine,ymine,zmine,xmaxe,ymaxe,zmaxe,aaa
          integer first,new,last,ierror
          integer :: nb_voxel_on !< number of remote nodes in the bounding box
          integer, dimension(:), allocatable :: list_nb_voxel_on
          integer :: node_per_cell !< number of nodes in a cell
          my_real :: x1, x2, x3, x4 !< coordinates of the 4 nodes of the segment (redundancy possible if triangle)
          my_real :: y1, y2, y3, y4
          my_real :: z1, z2, z3, z4
          my_real, dimension(:), allocatable :: xi, yi, zi     !< coordinates of the secondary node
          my_real, dimension(:), allocatable :: stif           !< stiffness of the nodes
          my_real, dimension(:), allocatable :: pene           !< penetration
          my_real, dimension(:), allocatable :: gapv           !< variable gap
          integer, dimension(:), allocatable :: prov_n, prov_e !< temporary list of candidates


C-----------------------------------------------
          IF(itask == 0)THEN
            allocate(list_nb_voxel_on((nbx+2)*(nby+2)*(nbz+2)))
            nb_voxel_on = 0
          END IF

!$OMP BARRIER

! The global bounding box contains all the nodes
! Some nodes may by higly distant from the impact zone
! The domain is subdivided in cells (voxel)
! All the cells have the sime size, except the first and the last one in each direction

!     bounding box of the model
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

c         ! En SPMD, pour inacti ou IFQ, retrouve ancienne numerotation des candidats non locaux
c         if(nspmd>1.and.(inacti==5.or.inacti==6.or.inacti==7.or.ifq>0.or.itied/=0)) then
c           CALL SPMD_OLDNUMCD(RENUM,OLDNUM,ISZNSNR,NSNROLD)
c         end if

C=======================================================================
C 1   Add local nodes to the cells
C=======================================================================
          if(itask==0.and.total_nb_nrtm>0)then
            allocate(last_nod(nsn+nsnr))
            do i=1,nsn
              iix=0
              iiy=0
              iiz=0
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
              iix=max(1,2+min(nbx,iix))
              iiy=max(1,2+min(nby,iiy))
              iiz=max(1,2+min(nbz,iiz))
              first = voxel(iix,iiy,iiz)
              if(first == 0)then
                !empty cell
                nb_voxel_on = nb_voxel_on + 1
                ! 1d version of 3d potition, indexes starts at 1
                list_nb_voxel_on(nb_voxel_on) = (iix-1)*(nby+2)*(nbz+2)+(iiy-1)*(nbz+2)+(iiz-1)
                voxel(iix,iiy,iiz) = i ! first
                next_nod(i)                 = 0 ! last one
                last_nod(i)                 = 0 ! no last
              elseif(last_nod(first) == 0)then
                !cell containing one node
                !add as next node
                next_nod(first) = i ! next
                last_nod(first) = i ! last
                next_nod(i)     = 0 ! last one
              else
                !
                !jump to the last node of the cell
                last = last_nod(first) ! last node in this voxel
                next_nod(last)  = i ! next
                last_nod(first) = i ! last
                next_nod(i)     = 0 ! last one
              endif
            enddo

C=======================================================================
C 2   Add remote (spmd) nodes to the cells
C=======================================================================
            do j = 1, nsnr

              if(xrem(1,j) < xmin)  cycle
              if(xrem(1,j) > xmax)  cycle
              if(xrem(2,j) < ymin)  cycle
              if(xrem(2,j) > ymax)  cycle
              if(xrem(3,j) < zmin)  cycle
              if(xrem(3,j) > zmax)  cycle

              iix=int(nbx*(xrem(1,j)-xminb)/(xmaxb-xminb))
              iiy=int(nby*(xrem(2,j)-yminb)/(ymaxb-yminb))
              iiz=int(nbz*(xrem(3,j)-zminb)/(zmaxb-zminb))
              iix=max(1,2+min(nbx,iix))
              iiy=max(1,2+min(nby,iiy))
              iiz=max(1,2+min(nbz,iiz))

              first = voxel(iix,iiy,iiz)

              if(first == 0)then
                ! empty cell
                nb_voxel_on = nb_voxel_on + 1
                ! 1d version of 3d potition
                list_nb_voxel_on( nb_voxel_on ) = (iix-1) * (nby+2) * (nbz+2) + (iiy-1) * (nbz+2) + (iiz-1)

                voxel(iix,iiy,iiz) = nsn+j ! first
                next_nod(nsn+j)     = 0 ! last one
                last_nod(nsn+j)     = 0 ! no last
              elseif(last_nod(first) == 0)then
                ! cell containing one node, add it as next node
                next_nod(first) = nsn+j  ! next
                last_nod(first) = nsn+j  ! last
                next_nod(nsn+j)  = 0     ! last one
              else
                ! , jump to the last node of the cell
                last = last_nod(first)  ! last node in this voxel
                next_nod(last)  = nsn+j ! next
                last_nod(first) = nsn+j ! last
                next_nod(nsn+j)     = 0 ! last one
              endif
            enddo
            deallocate(last_nod)
          end if !itask == 0

!$OMP BARRIER

          if(total_nb_nrtm>0 .and. itask == 0) then
            max_node_per_cell = 0
            do jj = 1, nb_voxel_on
              k = list_nb_voxel_on(jj)
              iiz =  mod(k,nbz+2) + 1
              ! iz [1,nbz+2]
              k = (k-iiz+1)/(nbz+2)
              iiy =  mod(k,nby+2) + 1
              k = (k-iiy+1)/(nby+2)
              iix = mod(k,nbx+2) + 1
              j = voxel(iix,iiy,iiz)
              node_per_cell = 0
              do while(j /= 0)
                node_per_cell = node_per_cell + 1
                j = next_nod(j)
              enddo
              max_node_per_cell = max(max_node_per_cell, node_per_cell)
            enddo
          endif

!$OMP BARRIER

          allocate(xi(max_node_per_cell))
          allocate(yi(max_node_per_cell))
          allocate(zi(max_node_per_cell))
          allocate(stif(max_node_per_cell))
          allocate(pene(max_node_per_cell))
          allocate(gapv(max_node_per_cell))
          allocate(prov_n(max_node_per_cell))
          allocate(prov_e(max_node_per_cell))

C=======================================================================
C 3   FACE RECOVERY AND ENUMERATION OF CANDIDATE COUPLES
C=======================================================================
          j_stok = 0
          if(flagremnode == 2) then
            allocate(tagremnode(numnod))
            do i=1,numnod
              tagremnode(i) = 0
            enddo
          endif
          call my_barrier
!$OMP DO SCHEDULE(DYNAMIC)
          do ne=1,nrtm
            if(stf(ne) == zero)cycle ! on ne retient pas les facettes detruites
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
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
                  if(i_mem==2) cycle
                  jj = voxel(ix,iy,iz)
                  do while(jj /= 0)

                    if(jj<=nsn)then
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
                      j=jj-nsn
                      delnod = 0
                      if(flagremnode == 2) then
                        nn = irem(1,j)
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

                    j_stok = j_stok + 1
                    prov_n(j_stok) = jj
                    prov_e(j_stok) = ne
  200               continue
                    jj = next_nod(jj)
                    if(i_mem==2) jj = 0
                  enddo ! while(jj /= 0)
                  if(j_stok > 0) call inter7_filter_cand(
     1                   j_stok,irect  ,x     ,nsv   ,ii_stok,
     2                   cand_n,cand_e ,mulnsn,marge  ,
     3                   i_mem ,prov_n ,prov_e,eshift,inacti ,
     4                   ifq   ,cand_a ,cand_p,ifpen ,nsn    ,
     5                   oldnum,nsnrold,igap  ,gap   ,gap_s  ,
     6                   gap_m ,gapmin ,gapmax,curv_max,
     7                   gap_s_l,gap_m_l,drad,itied    ,
     8                   cand_f ,dgapload,
     .                   x1, x2, x3, x4,
     .                   y1, y2, y3, y4,
     .                   z1, z2, z3, z4,
     .                   xi, yi, zi,
     .                   pene, gapv, nsnr,
     .                   xrem ,s_xrem)
                  j_stok = 0

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
!$OMP END DO
C=======================================================================
C 5   VOXEL CLUSTERING  RESET
C=======================================================================
!$OMP BARRIER
          IF(TOTAL_NB_NRTM>0 .AND. itask == 0) THEN
            DO JJ = 1, NB_VOXEL_ON
              J = list_nb_voxel_on(JJ)
              K = J
              IIZ =  MOD(K,NBZ+2) + 1
              ! IZ [1,NBZ+2]
              K = (K-IIZ+1)/(NBZ+2)
              IIY =  MOD(K,NBY+2) + 1
              K = (K-IIY+1)/(NBY+2)
              IIX = MOD(K,NBX+2) + 1
              VOXEL(IIX,IIY,IIZ) = 0
            ENDDO
          ENDIF
!$OMP BARRIER
C=======================================================================
C 7   DEALLOCATE
C=======================================================================
          IF(FLAGREMNODE == 2) THEN
            IF(ALLOCATED(TAGREMNODE)) DEALLOCATE(TAGREMNODE)
          ENDIF
          If(itask == 0) DEALLOCATE(LIST_NB_VOXEL_ON)
          DEALLOCATE(XI)
          DEALLOCATE(YI)
          DEALLOCATE(ZI)
          DEALLOCATE(STIF)
          DEALLOCATE(PENE)
          DEALLOCATE(GAPV)
          DEALLOCATE(PROV_N)
          DEALLOCATE(PROV_E)


          RETURN
        END

      END MODULE



