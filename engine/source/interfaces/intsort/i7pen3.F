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

Chd|====================================================================
Chd|  I7_PENETRATION                       source/interfaces/intsort/i7pen3.F
Chd|-- called by -----------
Chd|        I7STO                         source/interfaces/intsort/i7sto.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE I7_PENETRATION_MOD
        contains
!< computes the penetration between a chuck of secondary nodes and a main surface
      SUBROUTINE I7_PENETRATION(JLT   ,MARGE ,X1    ,X2     ,X3   ,
     .                  X4    ,Y1    ,Y2    ,Y3     ,Y4   ,
     .                  Z1    ,Z2    ,Z3    ,Z4     ,XI   ,
     .                  YI    ,ZI    ,PENE  ,GAPV  ,I3N)
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
      INTEGER JLT, IGAP
      INTEGER, INTENT(in) :: I3N ! 0 QUAD, 1 TRIANGLE
      my_real
     .        GAPV(JLT), MARGE 
      my_real
     .     X1, X2, X3, X4, !< X coordinates of the 4 nodes of the surface
     .     Y1, Y2, Y3, Y4, !< Y coordinates of the 4 nodes of the surface
     .     Z1, Z2, Z3, Z4, !< Z coordinates of the 4 nodes of the surface
     .     XI(JLT), YI(JLT), ZI(JLT), PENE(JLT) !< secondary nodes and penetration
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG
      my_real
     .     X0, Y0, Z0, GAP2(JLT),
     .     NX1, NX2, NX3, NX4,
     .     NY1, NY2, NY3, NY4,
     .     NZ1, NZ2, NZ3, NZ4,
     .     LB1(JLT), LB2(JLT), LB3(JLT), LB4(JLT),
     .     LC1(JLT), LC2(JLT), LC3(JLT), LC4(JLT),
     .     AL1(JLT), AL2(JLT), AL3(JLT), AL4(JLT),
     .     P1,  P2,  P3,  P4,
     .     X01,  X02,  X03, X04,
     .     Y01,  Y02,  Y03, Y04,
     .     Z01,  Z02,  Z03, Z04,
     .     XI1(JLT),  XI2(JLT),  XI3(JLT), XI4(JLT),
     .     YI1(JLT),  YI2(JLT),  YI3(JLT), YI4(JLT),
     .     ZI1(JLT),  ZI2(JLT),  ZI3(JLT), ZI4(JLT),
     .     HLB1(JLT), HLC1(JLT), HLB2(JLT),HLC2(JLT),
     .     HLB3(JLT), HLC3(JLT), HLB4(JLT),HLC4(JLT)
      my_real
     .     DTI, S2,A1,A2,A3,A4,D1,D2,D3,D4,
     .     X12,X23,X34,X41,XI0,SX1,SX2,SX3,SX4,SX0,
     .     Y12,Y23,Y34,Y41,YI0,SY1,SY2,SY3,SY4,SY0,
     .     Z12,Z23,Z34,Z41,ZI0,SZ1,SZ2,SZ3,SZ4,SZ0,
     .     LA, HLA, AAA, ZONEINF, ZONEINF2
C-----------------------------------------------
c GAPV(I)=GAP+CURV_MAX(CAND_E(I))
! the aim is to compute the penetration between a chuck of secondary nodes and a main surface
! the candidate for penetration are couple of secondary nodes and the main surface
! we keep the nodes that are in the zone of influence of the main surface
! we will perform a new collision detection when a node has moved more than the zone of influence (relatively to the main surface)
      DO I = 1, JLT
          ZONEINF = GAPV(I)+MARGE !< zone of influence: gap of the element + margin 
          GAP2(I)= ZONEINF*ZONEINF
      END DO
C
C--------------------------------------------------------
C   Quadrangle 
C--------------------------------------------------------
      IF(I3N==0) THEN
         X0 = FOURTH*(X1+X2+X3+X4)
         Y0 = FOURTH*(Y1+Y2+Y3+Y4)
         Z0 = FOURTH*(Z1+Z2+Z3+Z4) 
C--------------------------------------------------------
C  Triangle                      
C--------------------------------------------------------
      ELSE
         X0 = X3
         Y0 = Y3
         Z0 = Z3
      ENDIF
c     write(6,*) "I3N=",I3N
c     write(6,*) "X0=",X0,"Y0=",Y0,"Z0=",Z0
c     write(6,*) "X1=",X1,"Y1=",Y1,"Z1=",Z1
c     write(6,*) "X2=",X2,"Y2=",Y2,"Z2=",Z2
c     write(6,*) "X3=",X3,"Y3=",Y3,"Z3=",Z3
c     write(6,*) "X4=",X4,"Y4=",Y4,"Z4=",Z4
C--------------------------------------------------------
C  UNIQUEMENT POUR PAQUET DE TRIANGLE
C--------------------------------------------------------
      IF(I3N==1) THEN

       X01 = X1 - X3
       Y01 = Y1 - Y3
       Z01 = Z1 - Z3
C
       X02 = X2 - X3
       Y02 = Y2 - Y3
       Z02 = Z2 - Z3

       X12 = X2 - X1
       Y12 = Y2 - Y1
       Z12 = Z2 - Z1

       DO I=1,JLT
         XI0 = X3 - XI(I)
         YI0 = Y3 - YI(I)
         ZI0 = Z3 - ZI(I)
C      
         XI1(I) = X1 - XI(I)
         YI1(I) = Y1 - YI(I)
         ZI1(I) = Z1 - ZI(I)
C      
         XI2(I) = X2 - XI(I)
         YI2(I) = Y2 - YI(I)
         ZI2(I) = Z2 - ZI(I)
C      
         SX1 = YI0*ZI1(I) - ZI0*YI1(I)
         SY1 = ZI0*XI1(I) - XI0*ZI1(I)
         SZ1 = XI0*YI1(I) - YI0*XI1(I)
C      
         SX2 = YI0*ZI2(I) - ZI0*YI2(I) !
         SY2 = ZI0*XI2(I) - XI0*ZI2(I)
         SZ2 = XI0*YI2(I) - YI0*XI2(I)
C      
         SX0 = Y01*Z02 - Z01*Y02
         SY0 = Z01*X02 - X01*Z02
         SZ0 = X01*Y02 - Y01*X02
         S2 = 1./MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C      
         LB1(I) = -(SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
         LC1(I) =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
C      
         AAA    = ONE/MAX(EM30,X01*X01+Y01*Y01+Z01*Z01)
         HLC1(I)= LC1(I)*ABS(LC1(I))*AAA
         AL1(I) = -(XI0*X01+YI0*Y01+ZI0*Z01)*AAA
         AL1(I) = MAX(ZERO,MIN(ONE,AL1(I)))
         AAA    = ONE/MAX(EM30,X02*X02+Y02*Y02+Z02*Z02)
         HLB1(I)= LB1(I)*ABS(LB1(I))*AAA
         AL2(I) = -(XI0*X02+YI0*Y02+ZI0*Z02)*AAA
         AL2(I) = MAX(ZERO,MIN(ONE,AL2(I))) 
       ENDDO
C
      DO I=1,JLT
       LA = ONE - LB1(I) - LC1(I)
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB1(I).AND.HLA<=HLC1(I))THEN
         LB1(I) = (XI2(I)*X12+YI2(I)*Y12+ZI2(I)*Z12)*AAA
         LB1(I) = MAX(ZERO,MIN(ONE,LB1(I)))
         LC1(I) = ONE - LB1(I)
        ELSEIF(LB1(I)<ZERO.AND.
     +        HLB1(I)<=HLC1(I).AND.HLB1(I)<=HLA)THEN
         LB1(I) = ZERO
         LC1(I) = AL2(I)
        ELSEIF(LC1(I)<ZERO.AND.
     +        HLC1(I)<=HLA.AND.HLC1(I)<=HLB1(I))THEN
         LC1(I) = ZERO
         LB1(I) = AL1(I)
        ENDIF
      ENDDO
C
      DO I=1,JLT
C
       NX1 = XI(I)-(X3 + LB1(I)*X01 + LC1(I)*X02)
       NY1 = YI(I)-(Y3 + LB1(I)*Y01 + LC1(I)*Y02)
       NZ1 = ZI(I)-(Z3 + LB1(I)*Z01 + LC1(I)*Z02)
       P1 = NX1*NX1 + NY1*NY1 +NZ1*NZ1
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       PENE(I) = MAX(ZERO, GAP2(I) - P1)
c      write(6,*) PENE(I)
c      write(6,"(2Z20)") GAP2(I),P1

C
      ENDDO
C--------------------------------------------------------
      ELSE ! quad

        X01 = X1 - X0
        Y01 = Y1 - Y0
        Z01 = Z1 - Z0
C
        X02 = X2 - X0
        Y02 = Y2 - Y0
        Z02 = Z2 - Z0
C
        X03 = X3 - X0
        Y03 = Y3 - Y0
        Z03 = Z3 - Z0
C
        X04 = X4 - X0
        Y04 = Y4 - Y0
        Z04 = Z4 - Z0
C
        X12 = X2 - X1
        Y12 = Y2 - Y1
        Z12 = Z2 - Z1

        X23 = X3 - X2
        Y23 = Y3 - Y2
        Z23 = Z3 - Z2
        
        X34 = X4 - X3
        Y34 = Y4 - Y3
        Z34 = Z4 - Z3

        X41 = X1 - X4
        Y41 = Y1 - Y4
        Z41 = Z1 - Z4



       DO I=1,JLT
C
        XI0 = X0 - XI(I)
        YI0 = Y0 - YI(I)
        ZI0 = Z0 - ZI(I)
C
        XI1(I) = X1 - XI(I)
        YI1(I) = Y1 - YI(I)
        ZI1(I) = Z1 - ZI(I)
C
        XI2(I) = X2 - XI(I)
        YI2(I) = Y2 - YI(I)
        ZI2(I) = Z2 - ZI(I)
C
        XI3(I) = X3 - XI(I)
        YI3(I) = Y3 - YI(I)
        ZI3(I) = Z3 - ZI(I)
C
        XI4(I) = X4 - XI(I)
        YI4(I) = Y4 - YI(I)
        ZI4(I) = Z4 - ZI(I)
C
        SX1 = YI0*ZI1(I) - ZI0*YI1(I)
        SY1 = ZI0*XI1(I) - XI0*ZI1(I)
        SZ1 = XI0*YI1(I) - YI0*XI1(I)
C
        SX2 = YI0*ZI2(I) - ZI0*YI2(I)
        SY2 = ZI0*XI2(I) - XI0*ZI2(I)
        SZ2 = XI0*YI2(I) - YI0*XI2(I)
C
        SX0 = Y01*Z02 - Z01*Y02
        SY0 = Z01*X02 - X01*Z02
        SZ0 = X01*Y02 - Y01*X02
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB1(I) = -(SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
        LC1(I) =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
C
        SX3 = YI0*ZI3(I) - ZI0*YI3(I)
        SY3 = ZI0*XI3(I) - XI0*ZI3(I)
        SZ3 = XI0*YI3(I) - YI0*XI3(I)
C
        SX0 = Y02*Z03 - Z02*Y03
        SY0 = Z02*X03 - X02*Z03
        SZ0 = X02*Y03 - Y02*X03
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB2(I) = -(SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
        LC2(I) =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
C
        SX4 = YI0*ZI4(I) - ZI0*YI4(I)
        SY4 = ZI0*XI4(I) - XI0*ZI4(I)
        SZ4 = XI0*YI4(I) - YI0*XI4(I)
C
        SX0 = Y03*Z04 - Z03*Y04
        SY0 = Z03*X04 - X03*Z04
        SZ0 = X03*Y04 - Y03*X04
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB3(I) = -(SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
        LC3(I) =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
C
        SX0 = Y04*Z01 - Z04*Y01
        SY0 = Z04*X01 - X04*Z01
        SZ0 = X04*Y01 - Y04*X01
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB4(I) = -(SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
        LC4(I) =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
C
        AAA    = ONE/MAX(EM30,X01*X01+Y01*Y01+Z01*Z01)
        HLC1(I)= LC1(I)*ABS(LC1(I))*AAA
        HLB4(I)= LB4(I)*ABS(LB4(I))*AAA
        AL1(I) = -(XI0*X01+YI0*Y01+ZI0*Z01)*AAA
        AL1(I) = MAX(ZERO,MIN(ONE,AL1(I)))
        AAA    = ONE/MAX(EM30,X02*X02+Y02*Y02+Z02*Z02)
        HLC2(I)= LC2(I)*ABS(LC2(I))*AAA
        HLB1(I)= LB1(I)*ABS(LB1(I))*AAA
        AL2(I) = -(XI0*X02+YI0*Y02+ZI0*Z02)*AAA
        AL2(I) = MAX(ZERO,MIN(ONE,AL2(I)))
        AAA    = ONE/MAX(EM30,X03*X03+Y03*Y03+Z03*Z03)
        HLC3(I)= LC3(I)*ABS(LC3(I))*AAA
        HLB2(I)= LB2(I)*ABS(LB2(I))*AAA
        AL3(I) = -(XI0*X03+YI0*Y03+ZI0*Z03)*AAA
        AL3(I) = MAX(ZERO,MIN(ONE,AL3(I)))
        AAA    = ONE/MAX(EM30,X04*X04+Y04*Y04+Z04*Z04)
        HLC4(I)= LC4(I)*ABS(LC4(I))*AAA
        HLB3(I)= LB3(I)*ABS(LB3(I))*AAA
        AL4(I) = -(XI0*X04+YI0*Y04+ZI0*Z04)*AAA
        AL4(I) = MAX(ZERO,MIN(ONE,AL4(I)))
C
       ENDDO
C
       DO I=1,JLT
        LA = ONE - LB1(I) - LC1(I)
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB1(I).AND.HLA<=HLC1(I))THEN
         LB1(I) = (XI2(I)*X12+YI2(I)*Y12+ZI2(I)*Z12) * AAA
         LB1(I) = MAX(ZERO,MIN(ONE,LB1(I)))
         LC1(I) = ONE - LB1(I)
        ELSEIF(LB1(I)<ZERO.AND.
     +         HLB1(I)<=HLC1(I).AND.HLB1(I)<=HLA)THEN
         LB1(I) = ZERO
         LC1(I) = AL2(I)
        ELSEIF(LC1(I)<ZERO.AND.
     +         HLC1(I)<=HLA.AND.HLC1(I)<=HLB1(I))THEN
         LC1(I) = ZERO
         LB1(I) = AL1(I)
        ENDIF
       ENDDO
C
       DO I=1,JLT
        LA = ONE - LB2(I) - LC2(I)
        AAA = ONE / MAX(EM20,X23*X23+Y23*Y23+Z23*Z23)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB2(I).AND.HLA<=HLC2(I))THEN
         LB2(I) = (XI3(I)*X23+YI3(I)*Y23+ZI3(I)*Z23)*AAA
         LB2(I) = MAX(ZERO,MIN(ONE,LB2(I)))
         LC2(I) = ONE - LB2(I)
        ELSEIF(LB2(I)<ZERO.AND.
     +         HLB2(I)<=HLC2(I).AND.HLB2(I)<=HLA)THEN
         LB2(I) = ZERO
         LC2(I) = AL3(I)
        ELSEIF(LC2(I)<ZERO.AND.
     +         HLC2(I)<=HLA.AND.HLC2(I)<=HLB2(I))THEN
         LC2(I) = ZERO
         LB2(I) = AL2(I)
        ENDIF
       ENDDO
C
       DO I=1,JLT
        LA = ONE - LB3(I) - LC3(I)
        AAA = ONE / MAX(EM20,X34*X34+Y34*Y34+Z34*Z34)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB3(I).AND.HLA<=HLC3(I))THEN
         LB3(I) = (XI4(I)*X34+YI4(I)*Y34+ZI4(I)*Z34)*AAA
         LB3(I) = MAX(ZERO,MIN(ONE,LB3(I)))
         LC3(I) = ONE - LB3(I)
        ELSEIF(LB3(I)<ZERO.AND.
     +         HLB3(I)<=HLC3(I).AND.HLB3(I)<=HLA)THEN
         LB3(I) = ZERO
         LC3(I) = AL4(I)
        ELSEIF(LC3(I)<ZERO.AND.
     +         HLC3(I)<=HLA.AND.HLC3(I)<=HLB3(I))THEN
         LC3(I) = ZERO
         LB3(I) = AL3(I)
        ENDIF
       ENDDO
C
       DO I=1,JLT
        LA = ONE - LB4(I) - LC4(I)
        AAA = ONE / MAX(EM20,X41*X41+Y41*Y41+Z41*Z41)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB4(I).AND.HLA<=HLC4(I))THEN
         LB4(I) = (XI1(I)*X41+YI1(I)*Y41+ZI1(I)*Z41)*AAA
         LB4(I) = MAX(ZERO,MIN(ONE,LB4(I)))
         LC4(I) = ONE - LB4(I)
        ELSEIF(LB4(I)<ZERO.AND.
     +         HLB4(I)<=HLC4(I).AND.HLB4(I)<=HLA)THEN
         LB4(I) = ZERO
         LC4(I) = AL1(I)
        ELSEIF(LC4(I)<ZERO.AND.
     +         HLC4(I)<=HLA.AND.HLC4(I)<=HLB4(I))THEN
         LC4(I) = ZERO
         LB4(I) = AL4(I)
        ENDIF
       ENDDO

       DO I=1,JLT
C
        NX1 = XI(I)-(X0 + LB1(I)*X01 + LC1(I)*X02)
        NY1 = YI(I)-(Y0 + LB1(I)*Y01 + LC1(I)*Y02)
        NZ1 = ZI(I)-(Z0 + LB1(I)*Z01 + LC1(I)*Z02)
c       write(6,"(A,3Z20)") "XI=",XI(I),YI(I),ZI(I)
c       write(6,"(A,3Z20)") "X0=",X0,Y0,Z0
c       write(6,"(A,3Z20)") "X01=",X01,Y01,Z01
c       write(6,"(A,3Z20)") "X02=",X02,Y02,Z02


        P1 = NX1*NX1 + NY1*NY1 +NZ1*NZ1
        D1 = MAX(ZERO, GAP2(I) - P1)
c       write(6,"(A,Z20,Z20)") "P1=",P1,GAP2(I)
c       write(6,"(A,Z20)") "D1=",D1
c       write(6,"(A,Z20)") "LB1=",LB1(I)
c       write(6,"(A,Z20)") "LC1=",LC1(I)
c       write(6,"(A,Z20,A,Z20,A,Z20,A,Z20)") "X0=",X0,"Y0=",Y0,"Z0=",Z0,"XI=",XI(I)
C
        NX2 = XI(I)-(X0 + LB2(I)*X02 + LC2(I)*X03)
        NY2 = YI(I)-(Y0 + LB2(I)*Y02 + LC2(I)*Y03)
        NZ2 = ZI(I)-(Z0 + LB2(I)*Z02 + LC2(I)*Z03)
        P2 = NX2*NX2 + NY2*NY2 +NZ2*NZ2
        D2 = MAX(ZERO, GAP2(I) - P2)
C
        NX3 = XI(I)-(X0 + LB3(I)*X03 + LC3(I)*X04)
        NY3 = YI(I)-(Y0 + LB3(I)*Y03 + LC3(I)*Y04)
        NZ3 = ZI(I)-(Z0 + LB3(I)*Z03 + LC3(I)*Z04)
        P3 = NX3*NX3 + NY3*NY3 +NZ3*NZ3
        D3 = MAX(ZERO, GAP2(I) - P3)
C
        NX4 = XI(I)-(X0 + LB4(I)*X04 + LC4(I)*X01)
        NY4 = YI(I)-(Y0 + LB4(I)*Y04 + LC4(I)*Y01)
        NZ4 = ZI(I)-(Z0 + LB4(I)*Z04 + LC4(I)*Z01)
        P4 = NX4*NX4 + NY4*NY4 +NZ4*NZ4
        D4 = MAX(ZERO, GAP2(I) - P4)
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
        PENE(I) = MAX(D1,D2,D3,D4)
c       write(6,*) "PENE=",PENE(I)
c       write(6,"(A,Z20,A,Z20,A,Z20,A,Z20)") "D1=",D1,"D2=",D2,"D3=",D3,"D4=",D4
C
       ENDDO
      ENDIF
C
      RETURN
      END
      end module