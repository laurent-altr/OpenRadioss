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
      MODULE INTER7_GATHER_CAND_MOD
        CONTAINS
Chd|====================================================================
Chd|  I7COR3T                       source/interfaces/int07/i7cor3t.F
Chd|-- called by -----------
Chd|        I10STO                        source/interfaces/intsort/i10sto.F
Chd|        I7STO                         source/interfaces/intsort/i7sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE INTER7_GATHER_CAND(JLT     ,X     ,IRECT ,NSV   ,CAND_E ,
     1                   CAND_N  ,IGAP  ,GAP   ,X1    ,X2     ,
     2                   X3      ,X4    ,Y1    ,Y2    ,Y3     ,
     3                   Y4      ,Z1    ,Z2    ,Z3    ,Z4     ,
     4                   XI      ,YI    ,ZI    ,
     5                   NSN   ,GAP_S  ,
     6                   GAP_M   ,GAPV  ,GAPMAX,GAPMIN,CURV_MAX,
     7                   ITYP    ,GAP_S_L,GAP_M_L,
     8                   DRAD    ,DGAPLOAD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .    JLT,IDT, NOINT, NSN, IGAP ,ITYP
C     REAL
      my_real
     .   X(3,*), GAPV(*), GAP_S(*), GAP_M(*),CURV_MAX(*),
     .   GAP, GAPMAX, GAPMIN
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .   X1, X2, X3, X4,
     .   Y1, Y2, Y3, Y4,
     .   Z1, Z2, Z3, Z4,
     .   XI(JLT), YI(JLT), ZI(JLT), 
     .   GAP_S_L(*),GAP_M_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX1, IX2, IX3, IX4
      INTEGER I ,J  ,IL, L, NN, IG,JFT,IADD
C-----------------------------------------------
      IF(IGAP==0)THEN
        !computes 
        DO I=1,JLT
          GAPV(I)=MAX(GAP+DGAPLOAD,DRAD)
        END DO
      ELSEIF(IGAP == 3)THEN 
        IADD = 9 
        DO I=1,JLT
          J = CAND_N(I)
          IF(J<=NSN) THEN 
            GAPV(I)=GAP_S(J)+GAP_M(CAND_E(I))
            GAPV(I)=MIN(GAP_S_L(J)+GAP_M_L(CAND_E(I)),GAPV(I))
          ELSE
            IG = J-NSN
            GAPV(I)=XREM(9,IG)+GAP_M(CAND_E(I))
            GAPV(I)=MIN(XREM(10,IG)+GAP_M_L(CAND_E(I)),GAPV(I))
          END IF
          GAPV(I)=MIN(GAPV(I),GAPMAX)
          GAPV(I)=MAX(GAPMIN,GAPV(I))
          GAPV(I)=MAX(DRAD,GAPV(I)+DGAPLOAD)
        END DO
      ELSE
        DO I=1,JLT
          J = CAND_N(I)
          IF(J<=NSN) THEN
            GAPV(I)=GAP_S(J)+GAP_M(CAND_E(I)) 
          ELSE
            IG = J-NSN
            GAPV(I)=XREM(9,IG)+GAP_M(CAND_E(I))
          END IF
          GAPV(I)=MIN(GAPV(I),GAPMAX)
          GAPV(I)=MAX(GAPMIN,GAPV(I))
          GAPV(I)=MAX(DRAD,GAPV(I)+DGAPLOAD)
        END DO
      END IF
      DO I=1,JLT
        J = CAND_N(I)
        IF(J<=NSN) THEN
          IG = NSV(J)
          XI(I) = X(1,IG)
          YI(I) = X(2,IG)
          ZI(I) = X(3,IG)
        ELSE
          IG = J-NSN
          XI(I) = XREM(1,IG)
          YI(I) = XREM(2,IG)
          ZI(I) = XREM(3,IG)
        ENDIF
C
        L  = CAND_E(I)
C
      ENDDO
        IX1=IRECT(1,L)
        X1=X(1,IX1)
        Y1=X(2,IX1)
        Z1=X(3,IX1)
C
        IX2=IRECT(2,L)
        X2=X(1,IX2)
        Y2=X(2,IX2)
        Z2=X(3,IX2)
C
        IX3=IRECT(3,L)
        X3=X(1,IX3)
        Y3=X(2,IX3)
        Z3=X(3,IX3)
C
        IX4=IRECT(4,L)
        X4=X(1,IX4)
        Y4=X(2,IX4)
        Z4=X(3,IX4)

      IF(ITYP == 7)THEN
        DO I=1,JLT
          GAPV(I) = GAPV(I) + CURV_MAX(CAND_E(I))
        END DO
      ENDIF
C
      RETURN
      END
      END MODULE

