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
      !||    spmd_cell_size_exchange   ../engine/source/mpi/interfaces/spmd_cell_size_exchange.F
      !||--- called by ------------------------------------------------------
      !||    spmd_cell_list_exchange   ../engine/source/mpi/interfaces/spmd_cell_list_exchange.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                    ../engine/source/output/message/message.F
      !||    arret                     ../engine/source/system/arret.F
      !||    my_orders                 ../common_source/tools/sort/my_orders.c
      !||    myqsort_int               ../common_source/tools/sort/myqsort_int.F
      !||--- uses       -----------------------------------------------------
      !||    inter_sorting_mod         ../engine/share/modules/inter_sorting_mod.F
      !||    message_mod               ../engine/share/message_module/message_mod.F
      !||    tri7box                   ../engine/share/modules/tri7box.F
      !||====================================================================
        SUBROUTINE SPMD_FILL_XREM(IRCVFROM,ISENDTO,WEIGHT,NSNFIOLD,INTER_STRUCT,
     .                    IAD_ELEM,FR_ELEM,X,V,MS,TEMP,KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,NIN,
     .                    SORT_COMM,NODNX_SMS_SIZ,TEMP_SIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE MESSAGE_MOD
        USE INTER_SORTING_MOD
        USE TRI7BOX
        USE INTER_STRUCT_MOD
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
#include      "sms_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NIN
        INTEGER, INTENT(in) :: NODNX_SMS_SIZ ! size of NODNX_SMS
        INTEGER, INTENT(in) :: TEMP_SIZE     ! size of TEMP
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) ::  IPARI
        INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRCVFROM
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: WEIGHT
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM
        INTEGER, INTENT(inout) :: FR_ELEM(*)
        my_real, DIMENSION(3,NUMNOD), INTENT(in) :: X,V
        my_real, DIMENSION(NUMNOD), INTENT(in) :: MS
        my_real, DIMENSION(TEMP_SIZE), INTENT(in) :: TEMP
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: KINET ! k energy 
        INTEGER, DIMENSION(NODNX_SMS_SIZ), INTENT(in) :: NODNX_SMS ! SMS array
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(in) :: INTBUF_TAB
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface

        INTEGER, DIMENSION(NSPMD), INTENT(inOUT) :: NSNFIOLD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
#ifdef MPI        
        INTEGER :: I,J,NOD,L,L2,KK,JJ,IJK,KJI
        INTEGER :: P,P_LOC,PWORLD
        INTEGER :: ADRESS,SHIFT_
        INTEGER :: ISIZ,RSIZ,IDEB,JDEB
        INTEGER :: NSN,NMN,IGAP,INTTH,INTFRIC,ITYP,ITIED
        INTEGER :: IFQ,INACTI

        INTEGER IERROR1,STATUS(MPI_STATUS_SIZE),IERROR

        INTEGER :: LOC_PROC
        INTEGER :: IX,IY,IZ,NB
        INTEGER, DIMENSION(NUMNOD) :: INDEX
        INTEGER :: ISHIFT,RSHIFT

        INTEGER :: MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5
        INTEGER :: MSGTYP,INFO

        INTEGER :: ERROR_SORT
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_2,ITRI
        INTEGER, DIMENSION(:), ALLOCATABLE ::  WORK
        INTEGER :: CELL_X_ID,CELL_Y_ID,CELL_Z_ID
        INTEGER :: DISPL
        LOGICAL :: NEED_TO_RCV

        INTEGER :: NB_INDEX_ALREADY_SEND,VALUE,NB_SAVE
        INTEGER :: MAX_NSNR,SUM_NSNR,COMM
        my_real :: XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX
!   --------------------------------------------------------------------  
        DATA MSGOFF/6021/
        DATA MSGOFF2/6022/
        DATA MSGOFF3/6023/
        DATA MSGOFF4/6024/ 
        DATA MSGOFF5/6025/ 
        
        !CALL SPMD_INTER7_XREM_BUFFER(IRCVFROM,ISENDTO,IPARI,NIN,INTER_STRUCT,SORT_COMM)

        LOC_PROC = ISPMD + 1
        RSIZ = SORT_COMM(NIN)%RSIZ
        ISIZ = SORT_COMM(NIN)%ISIZ

        IGAP = IPARI(21,NIN)
        INTTH = IPARI(47,NIN)
        INTFRIC = IPARI(72,NIN)
        ITYP = IPARI(7,NIN)
        ITIED = IPARI(85,NIN)
        NMN = IPARI(6,NIN)
        NSN = IPARI(5,NIN)
        INACTI = IPARI(22,NIN)
        IFQ =IPARI(31,NIN)
        NB_INDEX_ALREADY_SEND= 0
        ! ---------------------------------
        IF(IRCVFROM(NIN,LOC_PROC)/=0.OR.ISENDTO(NIN,LOC_PROC)/=0) THEN
            ! ---------------------------------
            ! only the proc with secondary nodes send theirs data
            IF(ISENDTO(NIN,LOC_PROC)/=0) THEN   !   local nsn >0

                P=LOC_PROC                                                      
                ! ----------------------------
                ! skip the frontier nodes with weight = 0
                ! ----------------------------

                SORT_COMM(NIN)%NB(P) = 0
                NB = 0
                !   --------------------------
                !   itied/=0 --> need to send all nodes 
                DO I=1,NSN
                    NOD = INTBUF_TAB(NIN)%NSV(I)
                    IF(WEIGHT(NOD)==1)THEN
                            NB = NB + 1
                            INDEX(NB) = I
                    ENDIF
                ENDDO
                !   save the number of secondary nodes:
                SORT_COMM(NIN)%NB(P) = NB
                !   --------------------------
                !   buffer allocation & buffer initialization
                IF (NB>0) THEN
                    ALLOCATE( SORT_COMM(NIN)%DATA_PROC(1)%RBUF(RSIZ*NB),STAT=IERROR)
                    ALLOCATE( SORT_COMM(NIN)%DATA_PROC(1)%IBUF(ISIZ*NB),STAT=IERROR)
                    IF(IERROR/=0) THEN
                        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                        CALL ARRET(2)
                    ENDIF

                    L = 0
                    L2= 0            
              
#include      "vectorize.inc"
                    DO J = 1, NB
                        I = INDEX(J)
                        NOD = INTBUF_TAB(NIN)%NSV(I)
                        SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+1) = X(1,NOD)
                        SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+2) = X(2,NOD)
                        SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+3) = X(3,NOD)
                        SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+4) = V(1,NOD)
                        SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+5) = V(2,NOD)
                        SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+6) = V(3,NOD)
                        SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+7) = MS(NOD)
                        SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+8) = INTBUF_TAB(NIN)%STFNS(I)          
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+1) = I
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+2) = ITAB(NOD)        
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+3) = KINET(NOD)
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+4) = 0 !IGAPXREMP
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+5) = 0 !I24XREMP        
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+6) = 0 !I24IREMP
                        L = L + RSIZ
                        L2 = L2 + ISIZ
                    END DO

c shift for real variables (prepare for next setting)            
                    RSHIFT = 9
c shift for integer variables (prepare for next setting) 
                    ISHIFT = 7 

c specific cases
c IGAP=1 or IGAP=2                 
                    IF(IGAP==1 .OR. IGAP==2)THEN
                        L = 0            
                        IGAPXREMP = RSHIFT        
#include      "vectorize.inc"               
                        DO J = 1, NB
                            I = INDEX(J)   
                            SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+RSHIFT)= INTBUF_TAB(NIN)%GAP_S(I)
                            L = L + RSIZ           
                        ENDDO
                        RSHIFT = RSHIFT + 1                      
c IGAP=3                       
                    ELSEIF(IGAP==3)THEN 
                        L = 0         
                        IGAPXREMP = RSHIFT        
#include      "vectorize.inc"                 
                        DO J = 1, NB
                            I = INDEX(J)
                            SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+RSHIFT)  = INTBUF_TAB(NIN)%GAP_S(I)
                            SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+RSHIFT+1)= INTBUF_TAB(NIN)%GAP_SL(I)
                            L = L + RSIZ
                        END DO
                        RSHIFT = RSHIFT + 2
                    ENDIF
                         
C thermic
                    IF(INTTH>0)THEN
                        L = 0
                        L2 = 0                     
#include      "vectorize.inc"                                          
                        DO J = 1, NB
                            I = INDEX(J)
                            NOD = INTBUF_TAB(NIN)%NSV(I)
                            SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+RSHIFT)   = TEMP(NOD)
                            SORT_COMM(NIN)%DATA_PROC(1)%RBUF(L+RSHIFT+1) = INTBUF_TAB(NIN)%AREAS(I)
                            SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+ISHIFT) = INTBUF_TAB(NIN)%IELEC(I)
                            L = L + RSIZ
                            L2 = L2 + ISIZ
                        END DO
                        RSHIFT = RSHIFT + 2
                        ISHIFT = ISHIFT + 1               
                    ENDIF
C Friction
                    IF(INTFRIC>0)THEN
                        L2 = 0                     
#include      "vectorize.inc"                                          
                        DO J = 1, NB
                            I = INDEX(J)
                            SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+ISHIFT) = INTBUF_TAB(NIN)%IPARTFRICS(I)
                            L2 = L2 + ISIZ
                        END DO
                        ISHIFT = ISHIFT + 1               
                    ENDIF              
C -- IDTMINS==2
                    IF(IDTMINS==2)THEN
                        L2 = 0
#include      "vectorize.inc"                              
                        DO J = 1, NB
                            I = INDEX(J)
                            NOD = INTBUF_TAB(NIN)%NSV(I)
                            SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+ISHIFT)  = NODNX_SMS(NOD)
                            SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+ISHIFT+1)= NOD
                            L2 = L2 + ISIZ
                        END DO
                        ISHIFT = ISHIFT + 2              
C -- IDTMINS_INT /= 0               
                    ELSEIF(IDTMINS_INT/=0)THEN
                        L2 = 0                 
#include      "vectorize.inc"              
                        DO J = 1, NB
                            I = INDEX(J)
                            NOD = INTBUF_TAB(NIN)%NSV(I)
                            SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+ISHIFT)= NOD
                            L2 = L2 + ISIZ
                        END DO
                        ISHIFT = ISHIFT + 1               
                    ENDIF                           
            !save specifics IREM and XREM indexes for INT24 sorting
                    L2 = 0
#include      "vectorize.inc"
                    DO J = 1, NB
                        I = INDEX(J)
                        NOD = INTBUF_TAB(NIN)%NSV(I)
                        !save specifics IREM and XREM indexes for INT24 sorting
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+4) = IGAPXREMP
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+5) = I24XREMP
                        SORT_COMM(NIN)%DATA_PROC(1)%IBUF(L2+6) = I24IREMP
                        L2 = L2 + ISIZ
                    END DO  
                ENDIF   !   if nb/=0
                !   ------------------------------
            ENDIF      
            ! ---------------------------------
            ! maximum of INTBUF_TAB(NIN)%NSNS(1:NSPMD)
            MAX_NSNR = MAXVAL(INTBUF_TAB(NIN)%NSNS(1:NSPMD))
            SUM_NSNR = SUM(INTBUF_TAB(NIN)%NSNS(1:NSPMD))
            allocate(SORT_COMM(NIN)%DATA_PROC(2)%RBUF(RSIZ*MAX_NSNR),STAT=IERROR)
            allocate(SORT_COMM(NIN)%DATA_PROC(2)%IBUF(ISIZ*MAX_NSNR),STAT=IERROR)
            if(allocated(XREM)) deallocate(XREM)
            if(allocated(IREM)) deallocate(IREM)
            allocate(XREM(RSIZ,SUM_NSNR),STAT=IERROR)
            allocate(IREM(ISIZ,SUM_NSNR),STAT=IERROR)

           IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0
     .   .OR.ITIED/=0.OR.ITYP==23.OR.ITYP==24   
     .   .OR.ITYP==25) THEN
             DO P = 1, NSPMD
               NSNFIOLD(P) = NSNFI(NIN)%P(P)
            END DO
          END IF
 
            J = 0
            NSNFI(NIN)%P(1:NSPMD) = 0

            XMIN=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4)                                        
            YMIN=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5)           
            ZMIN=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6)           
            XMAX=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1)           
            YMAX=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2)           
            ZMAX=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3)           

            DO P = 1, INTBUF_TAB(NIN)%NSPMD
                PWORLD = INTBUF_TAB(NIN)%RANKS(P)
                KK = INTBUF_TAB(NIN)%NSNS(PWORLD) * RSIZ 
                JJ = INTBUF_TAB(NIN)%NSNS(PWORLD) * ISIZ 
                COMM = INTBUF_TAB(NIN)%MPI_COMM

                IF(P == INTBUF_TAB(NIN)%RANK+1) THEN  
                  CALL MPI_BCAST(SORT_COMM(NIN)%DATA_PROC(1)%RBUF,KK,REAL,P,COMM,IERROR) 
                  CALL MPI_BCAST(SORT_COMM(NIN)%DATA_PROC(1)%IBUF,JJ,MPI_INTEGER,P,COMM,IERROR) 

                ELSE
                  CALL MPI_BCAST(SORT_COMM(NIN)%DATA_PROC(2)%RBUF,KK,REAL,P,COMM,IERROR) 
    
                 ! fill xrem
                  CALL MPI_BCAST(SORT_COMM(NIN)%DATA_PROC(2)%IBUF,JJ,MPI_INTEGER,P,COMM,IERROR) 
                    ! fill irem
! on doit supprimer les noeuds en frontiere, et les noeuds en dehors de la boite de calcul
! Pour retirer les noeuds en frontière, on peut envoyer PMAIN(1:NUMNOD) = owner of each node
! des stif à supprimer aussi
                  DO I = 1,INTBUF_TAB(NIN)%NSNS(PWORLD)
                    XREM(1:3,J+I) = HUGE(XREM(1,1))
                    IF(SORT_COMM(NIN)%DATA_PROC(2)%RBUF(1) < XMIN)  CYCLE
                    IF(SORT_COMM(NIN)%DATA_PROC(2)%RBUF(1) > XMAX)  CYCLE
                    IF(SORT_COMM(NIN)%DATA_PROC(2)%RBUF(2) < YMIN)  CYCLE
                    IF(SORT_COMM(NIN)%DATA_PROC(2)%RBUF(2) > YMAX)  CYCLE
                    IF(SORT_COMM(NIN)%DATA_PROC(2)%RBUF(3) < ZMIN)  CYCLE
                    IF(SORT_COMM(NIN)%DATA_PROC(2)%RBUF(3) > ZMAX)  CYCLE
                    XREM(1:RSIZ,J+I) = SORT_COMM(NIN)%DATA_PROC(2)%RBUF(RSIZ*(I-1)+1:RSIZ*I)
                    IREM(1:ISIZ,J+I) = SORT_COMM(NIN)%DATA_PROC(2)%IBUF(ISIZ*(I-1)+1:ISIZ*I)
                  END DO
                  J = J + INTBUF_TAB(NIN)%NSNS(PWORLD)
                  NSNFI(NIN)%P(PWORLD) = INTBUF_TAB(NIN)%NSNS(PWORLD)
                ENDIF
            ENDDO
     
     
            IF(allocated(SORT_COMM(NIN)%DATA_PROC(1)%RBUF)) DEALLOCATE(SORT_COMM(NIN)%DATA_PROC(1)%RBUF)
            IF(allocated(SORT_COMM(NIN)%DATA_PROC(1)%IBUF)) DEALLOCATE(SORT_COMM(NIN)%DATA_PROC(1)%IBUF)
            IF(allocated(SORT_COMM(NIN)%DATA_PROC(2)%RBUF)) DEALLOCATE(SORT_COMM(NIN)%DATA_PROC(2)%RBUF)
            IF(allocated(SORT_COMM(NIN)%DATA_PROC(2)%IBUF)) DEALLOCATE(SORT_COMM(NIN)%DATA_PROC(2)%IBUF)
        ENDIF ! nsn>0 or nmn > 0

#endif
        RETURN
        END SUBROUTINE SPMD_FILL_XREM
