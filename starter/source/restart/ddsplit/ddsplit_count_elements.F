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
!||    ddsplit_count_elements           ../starter/source/restart/ddsplit/ddsplit_count_elements.F
!||--- called by ------------------------------------------------------
!||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    c_eltloc                         ../starter/source/restart/ddsplit/c_eltloc.F
!||    c_ngroup                         ../starter/source/restart/ddsplit/c_ngroup.F
!||====================================================================
      SUBROUTINE DDSPLIT_COUNT_ELEMENTS(
     1   IPARG       ,CEP         ,P           ,LEN_CEP     ,
     2   NUMELS8     ,NUMELS10    ,NUMELS20    ,NUMELS16    ,
     3   NUMELQ      ,NUMELC      ,NUMELT      ,NUMELP      ,
     4   NUMELR      ,NUMELTG     ,NUMELTG6    ,NUMELX      ,
     5   NUMELIG3D   ,NUMLOCGROUP ,NGROUP_L    ,NUMELS8_L   ,
     6   NUMELS10_L  ,NUMELS20_L  ,NUMELS16_L  ,NUMELS_L    ,
     7   NUMELQ_L    ,NUMELC_L    ,NUMELT_L    ,NUMELP_L    ,
     8   NUMELR_L    ,NUMELTG_L   ,NUMELTG6_L  ,NUMELTG3_L  ,
     9   NUMELX_L    ,NUMELIG3D_L ,NUM16SHIFT_L,PM1SHF      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: P, LEN_CEP
      INTEGER, INTENT(IN) :: NUMELS8, NUMELS10, NUMELS20, NUMELS16
      INTEGER, INTENT(IN) :: NUMELQ, NUMELC, NUMELT, NUMELP, NUMELR
      INTEGER, INTENT(IN) :: NUMELTG, NUMELTG6, NUMELX, NUMELIG3D
      INTEGER, INTENT(IN) :: CEP(LEN_CEP)
      INTEGER, INTENT(IN) :: IPARG(*)
      INTEGER, INTENT(OUT) :: NUMLOCGROUP(*), NGROUP_L
      INTEGER, INTENT(OUT) :: NUMELS8_L, NUMELS10_L, NUMELS20_L, NUMELS16_L
      INTEGER, INTENT(OUT) :: NUMELS_L, NUMELQ_L, NUMELC_L, NUMELT_L
      INTEGER, INTENT(OUT) :: NUMELP_L, NUMELR_L, NUMELTG_L, NUMELTG6_L
      INTEGER, INTENT(OUT) :: NUMELTG3_L, NUMELX_L, NUMELIG3D_L
      INTEGER, INTENT(OUT) :: NUM16SHIFT_L
      INTEGER, INTENT(INOUT) :: PM1SHF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: OFF, NUMELTG3
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      EXTERNAL C_ELTLOC, C_NGROUP
C-----------------------------------------------
C count local NGROUP 
C-----------------------------------------------
      CALL C_NGROUP(IPARG,P-1,NUMLOCGROUP,NGROUP_L)
C--------------------------------------------
c count local elements 
C--------------------------------------------
      OFF = 0
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS8,NUMELS8_L)
      OFF = OFF + NUMELS8
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS10,NUMELS10_L)
      OFF = OFF + NUMELS10
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS20,NUMELS20_L)
      OFF = OFF + NUMELS20
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS16,NUMELS16_L)
      OFF = OFF + NUMELS16
      NUMELS_L = NUMELS8_L + NUMELS10_L + NUMELS20_L + NUMELS16_L
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELQ,NUMELQ_L)
      OFF = OFF + NUMELQ
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L)
      OFF = OFF + NUMELC
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELT,NUMELT_L)
      OFF = OFF + NUMELT
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELP,NUMELP_L)
      OFF = OFF + NUMELP
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELR,NUMELR_L)
      OFF = OFF + NUMELR
      NUMELTG3=NUMELTG-NUMELTG6
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG3,NUMELTG_L)
      OFF = OFF + NUMELTG3
      NUMELTG3_L = NUMELTG_L
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG6,NUMELTG6_L)
      OFF = OFF + NUMELTG6
      NUMELTG_L = NUMELTG3_L+NUMELTG6_L
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELX,NUMELX_L)
      OFF = OFF + NUMELX
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELIG3D,NUMELIG3D_L)
      OFF = OFF + NUMELIG3D
C
      IF (P==1) THEN
        NUM16SHIFT_L = 1
        PM1SHF = 2*NUMELS16_L + 1
      ELSE
        NUM16SHIFT_L = PM1SHF
        PM1SHF =  PM1SHF + 2*NUMELS16_L
      ENDIF
C-----------------------------------------------
      RETURN
      END SUBROUTINE DDSPLIT_COUNT_ELEMENTS
