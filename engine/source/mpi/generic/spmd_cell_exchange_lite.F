Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    spmd_cell_exchange         ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||--- called by ------------------------------------------------------
      !||    inter_sort_07              ../engine/source/interfaces/int07/inter_sort_07.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                     ../engine/source/output/message/message.F
      !||    arret                      ../engine/source/system/arret.F
      !||    fill_voxel_local_partial   ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    fill_voxel_remote          ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    startime                   ../engine/source/system/timer_mod.F90
      !||    stoptime                   ../engine/source/system/timer_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    fill_voxel_mod             ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    inter_sorting_mod          ../engine/share/modules/inter_sorting_mod.F
      !||    inter_struct_mod           ../engine/share/modules/inter_struct_mod.F
      !||    message_mod                ../engine/share/message_module/message_mod.F
      !||    multi_fvm_mod              ../common_source/modules/ale/multi_fvm_mod.F90
      !||    timer_mod                  ../engine/source/system/timer_mod.F90
      !||    tri7box                    ../engine/share/modules/tri7box.F
      !||====================================================================
      SUBROUTINE SPMD_CELL_EXCHANGE_LITE(TIMERS, NIN,ISENDTO,IRCVFROM,NSN,NSNR,IGAP,
     1                               IFQ,INACTI,NSNFIOLD,INTTH,ITYP,STFNS, NSV,
     2                               NRTM, X, INTBUF_TAB,IPARI,NPARI,
     2                               ITIED,NMN,INTER_STRUCT,SORT_COMM, GOT_PREVIEW)
!$COMMENT
!       SPMD_CELL_EXCHANGE description :
!       exchange of secondary node data (x, v, temp...)
!       SPMD_CELL_EXCHANGE organization :
!       proc P needs to :
!                  * send data if local NSN > 0 & remote NMN > 0 (--> SORT_COMM(NIN)%NB(P)>0)
!                  * rcv data if local NMN > 0 & remote NSN > 0 (--> given by SORT_COMM(NIN)%NBIRECV)
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TIMER_MOD
        USE FILL_VOXEL_MOD
        USE TRI7BOX
        USE MESSAGE_MOD
        USE MULTI_FVM_MOD
        USE INTER_SORTING_MOD
        USE INTER_STRUCT_MOD
        USE INTBUFDEF_MOD
        USE EXTEND_ARRAY_MOD
        use umap_mod
        use MY_ALLOC_MOD
        use cppsort_mod
        use update_patch_mod, only : intersects
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(INTBUF_STRUCT_), INTENT(inout) :: INTBUF_TAB
        integer, intent(in) :: NPARI
        INTEGER, INTENT(in) :: IPARI(NPARI,NINTER)
        TYPE(TIMER_) :: TIMERS
        INTEGER NIN, IFQ, INACTI, IGAP,INTTH,NSN,NSNR,
     .        ITIED,
     .        NSNFIOLD(NSPMD),
     .        ISENDTO(NINTER+1,NSPMD+1), IRCVFROM(NINTER+1,NSPMD+1),
     .        ITYP
        INTEGER :: GOT_PREVIEW !< flag to indicate if -preview is available
        INTEGER :: NMN
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
        my_real :: stfns(nsn)
        INTEGER :: NSV(NSN)
        INTEGER :: NRTM
        my_real :: X(3,NUMNOD)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,NOD, DT_CST, LOC_PROC,P,IDEB,
     .        SIZ,J, L, BUFSIZ, LEN, NB, IERROR1, IAD,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),
     .        REQ_RC(NSPMD),REQ_SC(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5,
     .        L2, REQ_RD2(NSPMD),
     .        LEN2, RSHIFT, ISHIFT, ND, JDEB, Q, NBB

      INTEGER :: P_LOC
      INTEGER :: KEY,CODE
     
      DATA MSGOFF4/6026/ 
      DATA MSGOFF5/6027/ 
        
      my_real
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
      
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGNSNFI  
      my_real, DIMENSION(:,:), ALLOCATABLE :: XTMP
      INTEGER :: ADRESS, LOCAL_RANK
      INTEGER :: SIZE_S
      INTEGER :: RSIZ, ISIZ
      INTEGER :: INTFRIC
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C================================================================
      my_real :: xmin, xmax, ymin, ymax, zmin, zmax
      integer :: max_remote
      integer :: iix, iiy, iiz,k,color
      integer :: ielec_offset, temp_offset, areas_offset, gap_s_offset, gap_sl_offset, ipartfrics_offset
      integer :: x_offset, v_offset, ms_offset, stfns_offset
      integer :: ispmd_offset, itab_offset, IKINET_offset, i_offset, boundary_offset
      double precision,  dimension(:,:), pointer :: shared_real
      integer(kind=8), dimension(:,:), pointer :: shared_int 
      integer :: size_shared_data,MM
      integer :: NSNGLO, OFFSET,NSNROLD
      integer :: nsn_local
      integer :: count_extend
      my_real, dimension(:,:), allocatable :: R_PACKED
      integer(kind=8), dimension(:,:), allocatable :: I_PACKED
      integer(kind=8) :: M
      my_real :: x1, x2, x3
      integer :: pp
      double precision, dimension(3,2) :: main_box
      integer, dimension(:), allocatable :: indexes, rank_in_inter
      integer :: intersects_counter
C-----------------------------------------------
      size_shared_data = (inter_struct(nin)%WIN%RSIZ + inter_struct(nin)%WIN%ISIZ)!
      NSNGLO = inter_struct(nin)%WIN%NSN_GLOBAL
      call c_f_pointer(inter_struct(nin)%win%shared_base, shared_int,[ size_shared_data, nsnglo ] )
      call c_f_pointer(inter_struct(nin)%win%shared_base, shared_real,[ size_shared_data, nsnglo ] )


      ielec_offset = inter_struct(nin)%win%ielec_offset
      temp_offset = inter_struct(nin)%win%temp_offset
      areas_offset = inter_struct(nin)%win%areas_offset
      gap_s_offset = inter_struct(nin)%win%gap_s_offset
      gap_sl_offset = inter_struct(nin)%win%gap_sl_offset
      ipartfrics_offset = inter_struct(nin)%win%ipartfrics_offset
      ispmd_offset = inter_struct(nin)%win%ispmd_offset
      itab_offset = inter_struct(nin)%win%itab_offset
      IKINET_offset = inter_struct(nin)%win%IKINET_offset
      i_offset = inter_struct(nin)%win%i_offset
      x_offset = inter_struct(nin)%win%x_offset
      v_offset = inter_struct(nin)%win%v_offset
      ms_offset = inter_struct(nin)%win%ms_offset
      stfns_offset = inter_struct(nin)%win%stfns_offset 
      boundary_offset = inter_struct(nin)%win%boundary_offset

      LOC_PROC = ISPMD + 1
      ! save the old NSNFI values
      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0
     .   .OR.ITIED/=0.OR.ITYP==23.OR.ITYP==24   
     .   .OR.ITYP==25) THEN
         DO P = 1, NSPMD
           NSNFIOLD(P) = INTER_STRUCT(NIN)%NSNFIOLD(P)
         END DO
      END IF
      IGAP = IPARI(21,NIN)
      INTTH = IPARI(47,NIN)
      INTFRIC = IPARI(72,NIN)
      ITYP = IPARI(7,NIN)
      ITIED = IPARI(85,NIN)
      NMN = IPARI(6,NIN)
      NSN = IPARI(5,NIN)
      INACTI = IPARI(22,NIN)
      IFQ =IPARI(31,NIN)
      NSNROLD = IPARI(24,NIN)

      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.ITIED/=0) THEN
          IF(.NOT.ALLOCATED(INTER_STRUCT(NIN)%NSNFIOLD) ) THEN
              ALLOCATE(INTER_STRUCT(NIN)%NSNFIOLD(NSPMD))
              INTER_STRUCT(NIN)%NSNFIOLD(1:NSPMD) = 0
          ENDIF
          DO P = 1, NSPMD
              INTER_STRUCT(NIN)%NSNFIOLD(P) = NSNFI(NIN)%P(P)
          END DO
      END IF
            RSIZ = 8    
            ISIZ = 6

c specific cases 
c IGAP=1 or IGAP=2
            IF(IGAP==1 .OR. IGAP==2)THEN
                RSIZ = RSIZ + 1
c IGAP=3
            ELSEIF(IGAP==3)THEN
                RSIZ = RSIZ + 2
            ENDIF

CC thermic      
C            IF(INTTH > 0 ) THEN    
C                RSIZ = RSIZ + 2
C                      ISIZ = ISIZ + 1
C            ENDIF
CC Friction      
C            IF(INTFRIC > 0 ) THEN    
C                      ISIZ = ISIZ + 1
C            ENDIF

CC -- IDTMINS==2      
C            IF(IDTMINS == 2)THEN     
C                ISIZ = ISIZ + 2
CC -- IDTMINS_INT /= 0
C            ELSEIF(IDTMINS_INT/=0)THEN    
C                ISIZ = ISIZ + 1
C            END IF
C


      IF(.NOT. (IRCVFROM(NIN,LOC_PROC)==0.AND.ISENDTO(NIN,LOC_PROC)==0))  THEN

      xmax = inter_struct(nin)%box_limit_main(1)
      ymax = inter_struct(nin)%box_limit_main(2)
      zmax = inter_struct(nin)%box_limit_main(3)
      xmin = inter_struct(nin)%box_limit_main(4)  
      ymin = inter_struct(nin)%box_limit_main(5)
      zmin = inter_struct(nin)%box_limit_main(6)
      main_box(1:3,1) = (/xmin,ymin,zmin/)
      main_box(1:3,2) = (/xmax,ymax,zmax/)

C       INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1) = MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1),MAX_LIMIT_MAIN(1))
C       INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2) = MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2),MAX_LIMIT_MAIN(2))
C       INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3) = MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3),MAX_LIMIT_MAIN(3))
C       
C       INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4) = MIN(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4),MIN_LIMIT_MAIN(1))
C       INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5) = MIN(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5),MIN_LIMIT_MAIN(2))
C       INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6) = MIN(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6),MIN_LIMIT_MAIN(3))
        max_remote = MAX(INT(1.2D0*inter_struct(nin)%old_rem_size),2*(NMN+NSN))
        NSNR = 0
        SORT_COMM(NIN)%RSIZ = RSIZ
        SORT_COMM(NIN)%ISIZ = ISIZ

      IF(allocated(XREM))DEALLOCATE(XREM)
      IF(allocated(IREM))DEALLOCATE(IREM)
c     ALLOCATE(XREM(RSIZ,max_remote),STAT=IERROR)
c     ALLOCATE(IREM(ISIZ,max_remote),STAT=IERROR)          
      CALL MY_ALLOC(XREM,RSIZ,max_remote)
      CALL MY_ALLOC(IREM,ISIZ,max_remote)
      
      NSNR = 0
      allocate(rank_in_inter(NSPMD))
      rank_in_inter = -1
      DO P = 1, inter_struct(nin)%win%size_inter
        rank_in_inter(inter_struct(nin)%win%ranks_mapping(4,P)+1) = P
       enddo
      IF (IMONM > 0) CALL STARTIME(TIMERS,25)
        count_extend = 0
        OFFSET = 0
        K = maxval(inter_struct(nin)%WIN%nsns_in_world(1:NSPMD))

        allocate(R_PACKED(size_shared_data,K))
        allocate(indexes(k))
        !allocate(I_PACKED(size_shared_data,K))

        intersects_counter = 0
        PP = 0
        DO P = 1, NSPMD
          nsn_local = 0
          IF(P .NE. ISPMD + 1) THEN
            K = inter_struct(nin)%WIN%nsns_in_world(P)
            if(k > 0 ) then 
            PP = rank_in_inter(P) 
            if(PP < 0) write(6,*) "ERROR: rank_in_inter(P) < 0 ",P,PP
!           R_PACKED(1:size_shared_data,1:K) = shared_real(1:size_shared_data,OFFSET+1:OFFSET+K)
!           write(800+ispmd,*) NIN,P,"NSN:",inter_struct(nin)%win%nsns_in_world(P)
!           DO J=1,inter_struct(nin)%WIN%nsns_in_world(P) ! this might be the issue
!           I = J +  inter_struct(nin)%win%starting_node_of_proc(P) 

            l = 0
            Do color = 1, inter_struct(nin)%win%patchs(pp)%nb_clusters
                 if(intersects(main_box,inter_struct(nin)%win%patchs(pp)%clusters(color)%bounding_box)) then
             !  if(.true.) then
                DO j = 1,inter_struct(nin)%win%patchs(pp)%clusters(color)%numnod
                   I = inter_struct(nin)%win%patchs(pp)%clusters(color)%index_to_win(j) 
                   l = l + 1
                   indexes(l) = I
                  ! write(800+ispmd,*) "use:",PP,color,j,I

                ENDDO
                endif
            enddo 
            !if(l > 0) write(6,*) 'PROCESS ',ISPMD+1,' COLLIDES WITH',P,L
       
           ! if( l .ne. k) write(6,*) "ERROR: ",l,k," : ",P           
            ! the sorting is necessary, XREM should be sorted according to the remote NSV
            !write(800+ispmd,*) P,"INDX",INDEXES(1:l)

            call cppsort(l,indexes)
            
              DO j = 1,l
                I = indexes(j)                                                          
           !    write(800+ispmd,*) "1M=",I,shared_int(itab_offset,I)


                IIX=0
                IIY=0
                IIZ=0
                !C Optimisation // recherche les noeuds compris dans xmin xmax des
                !C elements du processeur
                X1 = shared_real(1,I)                          
                X2 = shared_real(2,I)
                X3 = shared_real(3,I)
!               X1 = R_PACKED(1,J)
!               X2 = R_PACKED(2,J)
!               X3 = R_PACKED(3,J)
!               write(800+ispmd,*) "1X=",X1, X2, X3

                IF(X1 < XMIN)  CYCLE
                IF(X1 > XMAX)  CYCLE
                IF(X2 < YMIN)  CYCLE
                IF(X2 > YMAX)  CYCLE
                IF(X3 < ZMIN)  CYCLE
                IF(X3 > ZMAX)  CYCLE
!               write(800+ispmd,*) "2X=",X1, X2, X3

                IF(shared_real(stfns_offset,I) == ZERO)CYCLE
!               IF(R_PACKED(stfns_offset,J) == ZERO)CYCLE
!               write(800+ispmd,*) "3X=",X1, X2, X3


                M = shared_int(boundary_offset,I) ! is the node at the boundary of multiple domains?
!               M = transfer(R_PACKED(boundary_offset,J),M) ! is the node at the boundary of multiple domains?
                if(m > 0 .and. nsn > 0) then
                   m = get_value_umap(inter_struct(nin)%win%boundary_to_local, I, -1) ! local number or -1
                   !write(800+ispmd,*) "2M=",m
                endif
                if(m > 0) then 
                  cycle  ! boundary node owned by another domain, but knowned by me 
                endif 
!               write(800+ispmd,*) "4X=",X1, X2, X3


                NSNR = NSNR + 1 
                nsn_local = nsn_local + 1
                if(nsnr == max_remote) then
                  max_remote = max(max_remote + 1000,int(max_remote*1.2))
!                 count_extend = count_extend + 1
                  call extend_array(XREM,RSIZ,nsnr,RSIZ,max_remote)
                  call extend_array(IREM,ISIZ,nsnr,ISIZ,max_remote)
                endif
                DO L=1,RSIZ
!                 XREM(L,NSNR) = R_PACKED(L,j)
                  XREM(L,NSNR) = shared_real(L,I)
                ENDDO
!               IREM(1,NSNR) = transfer(R_PACKED(i_offset,J),M) !shared_int(i_offset,I)
!               IREM(2,NSNR) = transfer(R_PACKED(itab_offset,J),M) ! shared_int(itab_offset,I)
!               IREM(3,NSNR) = transfer(R_PACKED(IKINET_offset,J),M)!  shared_int(IKINET_offset,I)
                IREM(1,NSNR) = shared_int(i_offset,I)
                IREM(2,NSNR) = shared_int(itab_offset,I)
                IREM(3,NSNR) = shared_int(IKINET_offset,I)

                IREM(4:ISIZ,NSNR) = 0 
                !write(800+ispmd,"(4I10)") P,NSNR, IREM(2,NSNR)
              ENDDO !j 
            ENDIF
           !ENDDO !color
          ENDIF ! ispmd
          OFFSET = OFFSET + inter_struct(nin)%WIN%nsns_in_world(P)
          NSNFI(NIN)%P(P) = nsn_local
        ENDDO ! P
        inter_struct(nin)%old_rem_size = NSNR
!       write(6,*) ispmd,"count_extend",count_extend,nsn,nsnr
        deallocate(R_PACKED) 
!       deallocate(I_PACKED)

       ENDIF

      IF (IMONM > 0) CALL STOPTIME(TIMERS,25)
#endif
      RETURN

      END SUBROUTINE SPMD_CELL_EXCHANGE_LITE
