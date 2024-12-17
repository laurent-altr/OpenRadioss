Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    s8zfint3   ../engine/source/elements/solid/solide8z/s8zfint3.F
      !||--- called by ------------------------------------------------------
      !||    s8cforc3   ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8zforc3   ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||====================================================================
      SUBROUTINE S8ZFINT3A(
     1   SIG,     SIGOR,
     .   PX, PXY, PXZ, PY, PYX, PYZ, PZ, PZX, PZY,
     .   BXY1,    BXY2,
     K   BXY3,    BXY4,    BXY5,    BXY6,
     L   BXY7,    BXY8,    BYX1,    BYX2,
     M   BYX3,    BYX4,    BYX5,    BYX6,
     N   BYX7,    BYX8,    BXZ1,    BXZ2,
     O   BXZ3,    BXZ4,    BXZ5,    BXZ6,
     P   BXZ7,    BXZ8,    BZX1,    BZX2,
     Q   BZX3,    BZX4,    BZX5,    BZX6,
     R   BZX7,    BZX8,    BYZ1,    BYZ2,
     S   BYZ3,    BYZ4,    BYZ5,    BYZ6,
     T   BYZ7,    BYZ8,    BZY1,    BZY2,
     U   BZY3,    BZY4,    BZY5,    BZY6,
     V   BZY7,    BZY8,    FX,FY,FZ,VOL,     QVIS,
     3   PP,      EINT,    RHO,     Q,
     4   DEFP,    EPSD,    EPSDM,   SIGM,
     5   EINTM,   RHOM,    QM,      DEFPM,
     6   VOLG,    STI,     STIN,    ICP,
     7   OFF,     VOL0,    VOL0G,   G_PLA,
     8   G_EPSD,  JFAC,    EINTTH,  EINTTHM,
     9   IEXPAN,  NEL,     IDEG,    CONDE,
     A   CONDEN,  MTN,     ISMSTR,  SVIS ,NODADT_THERM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: NODADT_THERM
      INTEGER ICP,G_PLA,G_EPSD,IEXPAN,NEL,IDEG(*)
C     REAL
      my_real
     .   SIG(NEL,6),SIGOR(NEL,6),
     .  PX(MVSIZ,8), PXY(MVSIZ,8), PXZ(MVSIZ,8),
     .  PY(MVSIZ,8), PYX(MVSIZ,8), PYZ(MVSIZ,8),
     .  PZ(MVSIZ,8), PZX(MVSIZ,8), PZY(MVSIZ,8),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   FX(8,MVSIZ), FY(8,MVSIZ), FZ(8,MVSIZ), 
     .   VOL(*),QVIS(*),PP(*),
     .   EINT(*),RHO(*),Q(*),DEFPM(*),DEFP(*),
     .   SIGM(NEL,6),EINTM(*),RHOM(*),QM(*),EPSD(*),EPSDM(*),
     .   VOLG(*),STI(*),STIN(*),OFF(*),VOL0(*),VOL0G(*),JFAC(*),
     .   EINTTH(*),EINTTHM(*),CONDE(MVSIZ),CONDEN(MVSIZ)
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),
     .   P(MVSIZ),FAC(MVSIZ),COEF,FVOL,
     .   QVIS_LOC,VOL_LOC
C-----------------------------------------------
      IF (ICP==1.AND.(ISMSTR==10.OR.ISMSTR==12)) THEN
       DO I=1,NEL
        QVIS_LOC = QVIS(I)
        FVOL=JFAC(I)*VOL(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS_LOC)*FVOL
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS_LOC)*FVOL
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS_LOC)*FVOL
        S4(I)=(SIG(I,4)+SVIS(I,4))*FVOL
        S5(I)=(SIG(I,5)+SVIS(I,5))*FVOL
        S6(I)=(SIG(I,6)+SVIS(I,6))*FVOL
       ENDDO
      ELSEIF (ICP==1) THEN
       COEF=THIRD
C------due to degratation of rubber test, will definitively fix w/ real Kt       
       IF (MTN==42.OR.MTN==62.OR.MTN==82)COEF=ZEP3
       DO I=1,NEL
        VOL_LOC = VOL(I)
        P(I) =COEF*(SIG(I,1)+SIG(I,2)+SIG(I,3)
     .            +SVIS(I,1)+SVIS(I,2)+SVIS(I,3))
        IF (IDEG(I)>10) P(I) =QVIS(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-P(I))*VOL_LOC
        S2(I)=(SIG(I,2)+SVIS(I,2)-P(I))*VOL_LOC
        S3(I)=(SIG(I,3)+SVIS(I,3)-P(I))*VOL_LOC
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL_LOC
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL_LOC
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL_LOC
       ENDDO
      ELSE
       DO I=1,NEL
        QVIS_LOC = QVIS(I)
        VOL_LOC = VOL(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS_LOC)*VOL_LOC
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS_LOC)*VOL_LOC
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS_LOC)*VOL_LOC
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL_LOC
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL_LOC
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL_LOC
       ENDDO
      ENDIF
       DO I=1,NEL
        FX(I,1)=FX(I,1)-(S1(I)*PX(I,1)+S4(I)*PXY(I,1)+S6(I)*PXZ(I,1))
        FX(I,2)=FX(I,2)-(S1(I)*PX(I,2)+S4(I)*PXY(I,2)+S6(I)*PXZ(I,2))
        FX(I,3)=FX(I,3)-(S1(I)*PX(I,3)+S4(I)*PXY(I,3)+S6(I)*PXZ(I,3))
        FX(I,4)=FX(I,4)-(S1(I)*PX(I,4)+S4(I)*PXY(I,4)+S6(I)*PXZ(I,4))
        FX(I,5)=FX(I,5)-(S1(I)*PX(I,5)+S4(I)*PXY(I,5)+S6(I)*PXZ(I,5))
        FX(I,6)=FX(I,6)-(S1(I)*PX(I,6)+S4(I)*PXY(I,6)+S6(I)*PXZ(I,6))
        FX(I,7)=FX(I,7)-(S1(I)*PX(I,7)+S4(I)*PXY(I,7)+S6(I)*PXZ(I,7))
        FX(I,8)=FX(I,8)-(S1(I)*PX(I,8)+S4(I)*PXY(I,8)+S6(I)*PXZ(I,8))
       ENDDO
       DO I=1,NEL
        FY(I,1)=FY(I,1)-(S2(I)*PY(I,1)+S4(I)*PYX(I,1)+S5(I)*PYZ(I,1))
        FY(I,2)=FY(I,2)-(S2(I)*PY(I,2)+S4(I)*PYX(I,2)+S5(I)*PYZ(I,2))
        FY(I,3)=FY(I,3)-(S2(I)*PY(I,3)+S4(I)*PYX(I,3)+S5(I)*PYZ(I,3))
        FY(I,4)=FY(I,4)-(S2(I)*PY(I,4)+S4(I)*PYX(I,4)+S5(I)*PYZ(I,4))
        FY(I,5)=FY(I,5)-(S2(I)*PY(I,5)+S4(I)*PYX(I,5)+S5(I)*PYZ(I,5))
        FY(I,6)=FY(I,6)-(S2(I)*PY(I,6)+S4(I)*PYX(I,6)+S5(I)*PYZ(I,6))
        FY(I,7)=FY(I,7)-(S2(I)*PY(I,7)+S4(I)*PYX(I,7)+S5(I)*PYZ(I,7))
        FY(I,8)=FY(I,8)-(S2(I)*PY(I,8)+S4(I)*PYX(I,8)+S5(I)*PYZ(I,8))
       ENDDO
       DO I=1,NEL
        FZ(I,1)=FZ(I,1)-(S3(I)*PZ(I,1)+S6(I)*PZX(I,1)+S5(I)*PZY(I,1))
        FZ(I,2)=FZ(I,2)-(S3(I)*PZ(I,2)+S6(I)*PZX(I,2)+S5(I)*PZY(I,2))
        FZ(I,3)=FZ(I,3)-(S3(I)*PZ(I,3)+S6(I)*PZX(I,3)+S5(I)*PZY(I,3))
        FZ(I,4)=FZ(I,4)-(S3(I)*PZ(I,4)+S6(I)*PZX(I,4)+S5(I)*PZY(I,4))
        FZ(I,5)=FZ(I,5)-(S3(I)*PZ(I,5)+S6(I)*PZX(I,5)+S5(I)*PZY(I,5))
        FZ(I,6)=FZ(I,6)-(S3(I)*PZ(I,6)+S6(I)*PZX(I,6)+S5(I)*PZY(I,6))
        FZ(I,7)=FZ(I,7)-(S3(I)*PZ(I,7)+S6(I)*PZX(I,7)+S5(I)*PZY(I,7))
        FZ(I,8)=FZ(I,8)-(S3(I)*PZ(I,8)+S6(I)*PZX(I,8)+S5(I)*PZY(I,8))
       ENDDO
C  
       IF (ICP/=1) THEN
        DO I=1,NEL
         FX(I,1)=FX(I,1)-(S2(I)*BXY1(I)+S3(I)*BXZ1(I))
         FY(I,1)=FY(I,1)-(S1(I)*BYX1(I)+S3(I)*BYZ1(I))
         FZ(I,1)=FZ(I,1)-(S1(I)*BZX1(I)+S2(I)*BZY1(I))
         FX(I,2)=FX(I,2)-(S2(I)*BXY2(I)+S3(I)*BXZ2(I))
         FY(I,2)=FY(I,2)-(S1(I)*BYX2(I)+S3(I)*BYZ2(I))
         FZ(I,2)=FZ(I,2)-(S1(I)*BZX2(I)+S2(I)*BZY2(I))
         FX(I,3)=FX(I,3)-(S2(I)*BXY3(I)+S3(I)*BXZ3(I))
         FY(I,3)=FY(I,3)-(S1(I)*BYX3(I)+S3(I)*BYZ3(I))
         FZ(I,3)=FZ(I,3)-(S1(I)*BZX3(I)+S2(I)*BZY3(I))
         FX(I,4)=FX(I,4)-(S2(I)*BXY4(I)+S3(I)*BXZ4(I))
         FY(I,4)=FY(I,4)-(S1(I)*BYX4(I)+S3(I)*BYZ4(I))
         FZ(I,4)=FZ(I,4)-(S1(I)*BZX4(I)+S2(I)*BZY4(I))
         FX(I,5)=FX(I,5)-(S2(I)*BXY5(I)+S3(I)*BXZ5(I))
         FY(I,5)=FY(I,5)-(S1(I)*BYX5(I)+S3(I)*BYZ5(I))
         FZ(I,5)=FZ(I,5)-(S1(I)*BZX5(I)+S2(I)*BZY5(I))
         FX(I,6)=FX(I,6)-(S2(I)*BXY6(I)+S3(I)*BXZ6(I))
         FY(I,6)=FY(I,6)-(S1(I)*BYX6(I)+S3(I)*BYZ6(I))
         FZ(I,6)=FZ(I,6)-(S1(I)*BZX6(I)+S2(I)*BZY6(I))
         FX(I,7)=FX(I,7)-(S2(I)*BXY7(I)+S3(I)*BXZ7(I))
         FY(I,7)=FY(I,7)-(S1(I)*BYX7(I)+S3(I)*BYZ7(I))
         FZ(I,7)=FZ(I,7)-(S1(I)*BZX7(I)+S2(I)*BZY7(I))
         FX(I,8)=FX(I,8)-(S2(I)*BXY8(I)+S3(I)*BXZ8(I))
         FY(I,8)=FY(I,8)-(S1(I)*BYX8(I)+S3(I)*BYZ8(I))
         FZ(I,8)=FZ(I,8)-(S1(I)*BZX8(I)+S2(I)*BZY8(I))
        ENDDO
       ENDIF
C----------------------------------------------    /
C   - post-traitement-valeur moyenne au sens a'=(_/  a dv ) /v
      DO I=1,NEL
         FAC(I) = OFF(I)*VOL(I)/VOLG(I)
         SIGM(I,1) = SIGM(I,1) + FAC(I) * SIGOR(I,1)
         SIGM(I,2) = SIGM(I,2) + FAC(I) * SIGOR(I,2)
         SIGM(I,3) = SIGM(I,3) + FAC(I) * SIGOR(I,3)
         SIGM(I,4) = SIGM(I,4) + FAC(I) * SIGOR(I,4)
         SIGM(I,5) = SIGM(I,5) + FAC(I) * SIGOR(I,5)
         SIGM(I,6) = SIGM(I,6) + FAC(I) * SIGOR(I,6)
         RHOM(I)   = RHOM(I)   + FAC(I) * RHO(I)
         EINTM(I)  = EINTM(I)  + EINT(I)*VOL0(I)/VOL0G(I)
         QM(I)     = QM(I)     + FAC(I) * Q(I)
C
         STIN(I)   = STIN(I)   + STI(I)
      ENDDO
      IF(NODADT_THERM == 1) THEN
        DO I=1,NEL
          CONDEN(I)= CONDEN(I)+ CONDE(I)
        ENDDO
      ENDIF
      IF (IEXPAN > 0) THEN
        DO I=1,NEL
          EINTTHM(I)= EINTTHM(I)+ EINTTH(I)*VOL0(I)/VOL0G(I)
        ENDDO
      ENDIF
      IF (G_PLA > 0) THEN
        DO I=1,NEL
          DEFPM(I)  = DEFPM(I)  + FAC(I) * DEFP(I)
        ENDDO
      ENDIF
      IF (G_EPSD > 0) THEN
        DO I=1,NEL
          EPSDM(I) = EPSDM(I) + FAC(I) * EPSD(I)
        ENDDO
      ENDIF
      IF (ICP == 1) THEN
        DO I=1,NEL
          PP(I) = PP(I) + FAC(I)* (P(I)-QVIS(I))
        ENDDO
      ENDIF
c-----------
      RETURN
      END
