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
!||    ddsplit_count_constraints        ../starter/source/restart/ddsplit/ddsplit_count_constraints.F
!||--- called by ------------------------------------------------------
!||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    c_irbe2                          ../starter/source/restart/ddsplit/c_irbe2.F
!||    c_irbe3                          ../starter/source/restart/ddsplit/c_irbe3.F
!||    c_rbyk                           ../starter/source/restart/ddsplit/c_rbyk.F
!||    c_rbymk                          ../starter/source/restart/ddsplit/c_rbymk.F
!||    c_sectio                         ../starter/source/restart/ddsplit/c_sectio.F
!||====================================================================
      SUBROUTINE DDSPLIT_COUNT_CONSTRAINTS(
     1   P           ,NRBYKIN     ,NPBY        ,LPBY        ,
     2   NRBYM       ,IRBYM       ,LCRBYM      ,NSECT       ,
     3   CEP         ,NRBE2       ,IRBE2       ,LRBE2       ,
     4   NRBE3       ,IRBE3       ,LRBE3       ,NSLARB_L    ,
     5   NSKYRBK_L   ,NSLARBM_L   ,NSKYRBMK_L  ,NSTRF_L     ,
     6   LSECBUF_L   ,NNODT_L     ,NNODL_L     ,NRBE2_L     ,
     7   LLRBE2_L    ,NBDDRBE2    ,NRBE3_L     ,LLRBE3_L    ,
     8   NBDDRBE3M   ,NSKYRBE3_L  ,NRBE3PEN_L  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: P, NRBYKIN, NRBYM, NSECT, NRBE2, NRBE3
      INTEGER, INTENT(IN) :: NPBY(*), LPBY(*), IRBYM(*), LCRBYM(*)
      INTEGER, INTENT(IN) :: CEP(*), IRBE2(*), LRBE2(*), IRBE3(*), LRBE3(*)
      INTEGER, INTENT(OUT) :: NSLARB_L, NSKYRBK_L, NSLARBM_L, NSKYRBMK_L
      INTEGER, INTENT(OUT) :: NSTRF_L, LSECBUF_L, NNODT_L, NNODL_L
      INTEGER, INTENT(OUT) :: NRBE2_L, LLRBE2_L, NBDDRBE2
      INTEGER, INTENT(OUT) :: NRBE3_L, LLRBE3_L, NBDDRBE3M, NSKYRBE3_L, NRBE3PEN_L
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      EXTERNAL C_RBYK, C_RBYMK, C_SECTIO, C_IRBE2, C_IRBE3
C-----------------------------------------------
C--------------------------------------------
C Count nodes rby
C--------------------------------------------
      NSLARB_L = 0
      NSKYRBK_L = 0
      IF(NRBYKIN>0)
     +  CALL C_RBYK(NPBY,LPBY,P-1,NSLARB_L,NSKYRBK_L)
C--------------------------------------------
C count Rigid material nodes
C--------------------------------------------
      NSLARBM_L = 0
      NSKYRBMK_L = 0
      IF(NRBYM > 0)
     +  CALL C_RBYMK(IRBYM,LCRBYM,P-1,NSLARBM_L,NSKYRBMK_L)
C--------------------------------------------
C count Sections
C--------------------------------------------
      NSTRF_L = 0
      LSECBUF_L = 0
      NNODT_L = 0
      NNODL_L = 0
      IF(NSECT>0)
     +  CALL C_SECTIO(CEP    ,P-1,NSTRF_L,
     +                LSECBUF_L,NNODT_L,NNODL_L)
C--------------------------------------
C counts RBE2
C--------------------------------------
      NRBE2_L=0
      LLRBE2_L=0
      NBDDRBE2 = 0
      IF (NRBE2>0)
     .   CALL C_IRBE2(IRBE2,LRBE2,P-1   ,NRBE2_L,
     .                LLRBE2_L ,NBDDRBE2)
C--------------------------------------
C counts RBE3
C--------------------------------------
      NRBE3_L=0
      LLRBE3_L=0
      NBDDRBE3M = 0
      NSKYRBE3_L = 0
      NRBE3PEN_L=0
      IF (NRBE3>0)
     .   CALL C_IRBE3(IRBE3,LRBE3,NSKYRBE3_L,P-1,
     .                NRBE3_L,LLRBE3_L ,NBDDRBE3M,NRBE3PEN_L)
C-----------------------------------------------
      RETURN
      END SUBROUTINE DDSPLIT_COUNT_CONSTRAINTS
