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
!hd|====================================================================
!hd|  INTER7_FILTER_CAND_MOD        source/interfaces/intsort/inter7_filter_cand.F
!hd|-- called by -----------
!hd|        INTER7_CANDIDATE_PAIRS        source/interfaces/intsort/inter7_candidate_pairs.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE INTER7_FILTER_CAND_MOD
      contains
!! \brief broad phase filtering of candidate pairs
!! \details input: nodes and segment sharing the same voxel, output: filtered candidate pairs
!hd|====================================================================
!hd|  INTER7_FILTER_CAND            source/interfaces/intsort/inter7_filter_cand.F
!hd|-- called by -----------
!hd|-- calls ---------------
!hd|        COLLISION_MOD                 source/interfaces/intsort/collision_mod.F
!hd|        INTER7_GATHER_CAND_MOD        source/interfaces/int07/inter7_gather_cand.F
!hd|        INTER7_PENETRATION_MOD        source/interfaces/intsort/inter7_penetration.F
!hd|====================================================================
        SUBROUTINE INTER7_FILTER_CAND(&
        &j_stok,irect  ,x     ,nsv   ,ii_stok,&
        &cand_n,cand_e ,mulnsn,margin  ,&
        &i_mem ,prov_n ,prov_e,eshift,inacti ,&
        &ifq   ,cand_a ,cand_p,ifpen ,nsn    ,&
        &oldnum,nsnrold,igap  ,gap   ,gap_s  ,&
        &gap_m ,gapmin ,gapmax,curv_max,&
        &gap_s_l,gap_m_l,drad,itied    ,&
        &cand_f ,dgapload,&
        &nsnr,&
        &xrem ,s_xrem)
          USE INTER7_GATHER_CAND_MOD , ONLY: INTER7_GATHER_CAND
          USE INTER7_PENETRATION_MOD , ONLY: INTER7_PENETRATION
          USE CONSTANT_MOD
          USE GPU_PREF
          USE PRECISION_MOD, ONLY: WP
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(inout) :: i_mem !< memory error flag
          integer, intent(in) :: nsn  !< number secondary nodes
          integer, intent(in) :: nsnrold !< number of remote nodes in the previous collision detection
          integer, intent(in) :: igap !< gap formulation
          integer, intent(in) :: itied !< tied contact formulation
          integer, intent(in) :: j_stok !< number of candidate pairs to be filtered
          integer, intent(in) :: mulnsn !< allocated size of cand_n, cand_e, cand_p, cand_f
          integer, intent(in) :: inacti !< initial penetration formulation
          integer, intent(in) :: ifq !< friction formulation ?
          integer, intent(in) :: eshift !< shift for segment numbering
          integer, intent(in) :: irect(4,*) !< 4 nodes of the segment
          integer, intent(in) :: nsv(*) !< secondary node ids
          integer, intent(inout) :: cand_n(*) !< output: node number of the candidate pair
          integer, intent(inout) :: cand_e(*) !< output: segment number of the candidate pair
          integer, intent(inout) :: cand_a(*)
          integer, intent(inout) :: prov_n(j_stok) !< input node number of the candidate pair, before filtering
          integer, intent(inout) :: prov_e(j_stok) !< input segment number of the candidate pair, before filtering
          integer, intent(inout) :: ifpen(*) !
          integer, intent(in) :: oldnum(*)
          integer, intent(inout) :: ii_stok !< current total number of candidate pairs
          real(WP), intent(in) :: drad
          real(WP), intent(in) :: dgapload
          real(WP), intent(in) :: x(3,*) !< coordinates of all the nodes
          real(WP), intent(inout) :: cand_p(*)
          real(WP), intent(in) :: gap_s(*)
          real(WP), intent(in) :: gap_m(*)
          real(WP), intent(in) :: margin
          real(WP), intent(in) :: gap
          real(WP), intent(in) :: gapmin
          real(WP), intent(in) :: gapmax
          real(WP), intent(in) :: curv_max(*)
          real(WP), intent(in) :: gap_s_l(*)
          real(WP), intent(in) :: gap_m_l(*)
          real(WP), intent(inout) :: cand_f(8,*)
          integer, intent(in) :: nsnr !< number of remote nodes
          integer, intent(in) :: s_xrem !< size of xrem
          real(WP), intent(in) :: xrem(s_xrem, nsnr)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i_stok_local,i_stok_atmic
          integer :: i,k_stok,i_stok,n,ne,j
          integer :: inacti_l, itied_l, ifq_l
          integer :: j_start, j_end
          integer, parameter :: itype = 7
          real(WP) :: x1(j_stok) !< x coordinate of the first node of the quadrangle or triangle
          real(WP) :: x2(j_stok)
          real(WP) :: x3(j_stok)
          real(WP) :: x4(j_stok)
          real(WP) :: y1(j_stok)
          real(WP) :: y2(j_stok)
          real(WP) :: y3(j_stok)
          real(WP) :: y4(j_stok)
          real(WP) :: z1(j_stok)
          real(WP) :: z2(j_stok)
          real(WP) :: z3(j_stok)
          real(WP) :: z4(j_stok)
          real(WP) :: xi(j_stok) !< x coordinate of the second nodes
          real(WP) :: yi(j_stok)
          real(WP) :: zi(j_stok)
          real(WP) :: pene(j_stok)
          real(WP) :: gapv(j_stok)
          integer :: ix1(j_stok)
          integer :: ix2(j_stok)
          integer :: ix3(j_stok)
          integer :: ix4(j_stok)
          logical :: exit_flag


!$ACC DATA CREATE(CAPTURE:x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,xi,yi,zi,gapv,ix1,ix2,ix3,ix4,pene)

!-----------------------------------------------

          !No local vars
          call nvtxStartRange("inter7_gather_cand")
          call inter7_gather_cand(j_stok  ,x    ,irect ,nsv   ,prov_e  ,&
          &prov_n  ,igap ,gap   ,x1    ,x2      ,&
          &x3      ,x4   ,y1    ,y2    ,y3      ,&
          &y4      ,z1   ,z2    ,z3    ,z4      ,&
          &xi      ,yi   ,zi    ,&
          &nsn     ,gap_s   , ix1, ix2, ix3, ix4,&
          &gap_m   ,gapv ,gapmax, gapmin, curv_max,&
          &itype   ,gap_s_l,gap_m_l,&
          &drad    ,dgapload, nsnr,&
          &s_xrem, xrem)
          call nvtxEndRange()

          call nvtxStartRange("inter7_penetration")
          call inter7_penetration(j_stok ,margin ,x1    ,x2     ,x3   ,&
          &x4    ,y1    ,y2    ,y3     ,y4   ,&
          &z1    ,z2    ,z3    ,z4     ,xi   ,&
          &ix3,   ix4,&
          &yi    ,zi    ,pene  ,gapv )
          call nvtxEndRange()


!-----------------------------------------------
! removal of old candidates already stored (initial penetration)
!-----------------------------------------------
          if(inacti==5.or.inacti==6.or.inacti==7.or.ifq>0.or.itied/=0)then
!$ACC PARALLEL LOOP GANG VECTOR DEFAULT(PRESENT)
            do i=1,j_stok
              if(pene(i)/=zero)then
                n  = prov_n(i)
                ne = prov_e(i)+eshift
                if(n>nsn) then
! numbering of previous collisions for remote nodes (spmd)
                  n = oldnum(n-nsn)+nsn
                  if(n==nsn) n = nsn+nsnrold+1
                end if
                j_start = cand_a(n)
                j_end = cand_a(n+1)-1
                do j = j_start, j_end
                  if(cand_e(j)==ne)then
                    pene(i)=zero
                    exit
                  endif
                enddo
              endif
            enddo
          endif
!-----------------------------------------------
          k_stok = 0
!$ACC PARALLEL LOOP REDUCTION(+:k_stok)
          do i=1,j_stok
            if(pene(i)/=zero) k_stok = k_stok + 1
          enddo


          !FIXME this causes issue when within a data region
          !if(k_stok==0)return
          exit_flag = .false.

!!$omp critical
          i_stok = ii_stok
          if (i_stok + k_stok > mulnsn) then
            i_mem = 2
            exit_flag = .true.
          else
            ii_stok = i_stok + k_stok
          endif

!!$omp end critical
          !FIXME this causes issue when within a data region
          !if (exit_flag) then
          !return
          !endif


          inacti_l = inacti
          itied_l = itied
          ifq_l = ifq
!$ACC PARALLEL LOOP COPY(i_stok)
          do i=1,j_stok
            if(pene(i)/=zero)then
!$ACC ATOMIC CAPTURE
              i_stok = i_stok + 1
              i_stok_local=i_stok
!$ACC END ATOMIC
              cand_n(i_stok_local) = prov_n(i)
              cand_e(i_stok_local) = prov_e(i)+eshift
              if(ifq_l > 0)ifpen(i_stok_local)  = 0
              if(inacti_l==5 .or. inacti_l==6 .or. inacti_l==7) cand_p(i_stok_local) = zero
              if(itied_l /= 0) cand_f(1:8,i_stok_local)=zero
            endif
          enddo

!$ACC END DATA
          return
        end
      END MODULE
