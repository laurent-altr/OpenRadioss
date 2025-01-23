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
      !||    inter_struct_init   ../engine/source/interfaces/generic/inter_struct_init.F
      !||--- called by ------------------------------------------------------
      !||    resol               ../engine/source/engine/resol.F
      !||--- uses       -----------------------------------------------------
      !||    inter_sorting_mod   ../engine/share/modules/inter_sorting_mod.F
      !||    inter_struct_mod    ../engine/share/modules/inter_struct_mod.F
      !||====================================================================
        SUBROUTINE INTER_STRUCT_INIT(INTER_STRUCT,SORT_COMM,INTBUF_TAB, IPARI,NPARI, ISPMD,WEIGHT)
!$COMMENT
!       INTER_STRUCT_INIT description
!       initialization of INTER_STRUCT structure
!       
!       INTER_STRUCT_INIT organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE INTER_STRUCT_MOD
        USE INTER_SORTING_MOD
        USE intbufdef_mod
        USE SPMD_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C------------------------o-----------------------
        TYPE(intbuf_struct_), INTENT(inout) :: INTBUF_TAB(NINTER)
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
        INTEGER, INTENT(in) :: NPARI !< number of parameters
        INTEGER, INTENT(in) :: IPARI(NPARI,NINTER) !< integer parameters
        INTEGER, INTENT(in) :: ISPMD !< current rank
        INTEGER, INTENT(in) :: WEIGHT(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: KK,NIN,i
        INTEGER :: NSN,NSN_OWEND,NMN, NRTM
        INTEGER :: SENDBUF(NSPMD)
        INTEGER :: KEY,COLOR
!   ----------------------------------------
        DO KK=1,NINTER
            INTER_STRUCT(KK)%SIZE_CAND_A = 0   
            INTER_STRUCT(KK)%I_SK_OLD = 0    
            INTER_STRUCT(KK)%INACTI = 0
            INTER_STRUCT(KK)%INACTII = 0
            INTER_STRUCT(KK)%CURV_MAX_MAX = ZERO
            INTER_STRUCT(KK)%NMN_G = 0                
            ALLOCATE(INTBUF_TAB(KK)%NSNS(NSPMD))
            NSN = IPARI(5,KK)
            NSN_OWEND = 0 
            DO i = 1,NSN
                IF(WEIGHT(intbuf_tab(kk)%NSV(i)) == 1) THEN
                    NSN_OWEND = NSN_OWEND + 1
                ENDIF
            ENDDO
            INTBUF_TAB(KK)%NSNS(1:NSPMD) = 0
            INTBUF_TAB(KK)%NSNS(ISPMD+1) = NSN_OWEND
            SENDBUF(1:NSPMD) = 0
            SENDBUF(ISPMD+1) = NSN_OWEND
            CALL SPMD_ALLREDUCE(SENDBUF, INTBUF_TAB(KK)%NSNS , NSPMD, SPMD_MAX)
                write(6,*)  "ISPMD=", INTBUF_TAB(KK)%NSNS(1:NSPMD)

        ENDDO
        DO KK=1,NINTER
            NSN = IPARI(5,KK)
            NRTM = IPARI(4,KK)
            NMN = IPARI(6,KK) 
            KEY = ISPMD
            COLOR = 0
            IF(NSN + NRTM + NMN > 0) COLOR = 1

#ifdef MPI
            CALL SPMD_COMM_SPLIT(COLOR,KEY,INTBUF_TAB(KK)%MPI_COMM,
     .                          INTBUF_TAB(KK)%RANK, INTBUF_TAB(KK)%NSPMD)
#endif
            ALLOCATE(INTBUF_TAB(KK)%RANKS(NSPMD))
            INTBUF_TAB(KK)%RANKS(1:NSPMD) = -1
            INTBUF_TAB(KK)%RANKS(  INTBUF_TAB(KK)%RANK  +1) = ISPMD
            IF(NSN+NRTM+NMN > 0) THEN
              CALL SPMD_ALLREDUCE(SENDBUF, INTBUF_TAB(KK)%RANKS , NSPMD, SPMD_MAX,COMM=INTBUF_TAB(KK)%MPI_COMM)
            ENDIF

       ENDDO
        DO KK=1,NB_INTER_7_INACTI
            NIN = LIST_INTER_7_INACTI(KK)
            ALLOCATE( INTER_STRUCT(NIN)%NSNFIOLD(NSPMD) )
            INTER_STRUCT(NIN)%NSNFIOLD(1:NSPMD) = 0
        ENDDO
        DO KK=1,NINTER
            SORT_COMM(KK)%PROC_NUMBER = 0
            SORT_COMM(KK)%SIZE_GLOBAL_COARSE_CELL = 0
            ALLOCATE( SORT_COMM(KK)%DATA_PROC(NSPMD) )
            ALLOCATE( SORT_COMM(KK)%REQ_SD2(NSPMD) )
            ALLOCATE( SORT_COMM(KK)%REQ_SD3(NSPMD) )
        ENDDO

        RETURN
        END SUBROUTINE INTER_STRUCT_INIT
