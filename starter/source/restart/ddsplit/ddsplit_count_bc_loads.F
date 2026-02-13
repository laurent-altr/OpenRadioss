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
!||    ddsplit_count_bc_loads          ../starter/source/restart/ddsplit/ddsplit_count_bc_loads.F
!||--- called by ------------------------------------------------------
!||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    c_ibcl                           ../starter/source/restart/ddsplit/c_ibcl.F
!||    c_ibfv                           ../starter/source/restart/ddsplit/c_ibfv.F
!||    c_ibvel                          ../starter/source/restart/ddsplit/c_ibvel.F
!||    c_igrav                          ../starter/source/restart/ddsplit/c_igrav.F
!||    c_llink                          ../starter/source/restart/ddsplit/c_llink.F
!||====================================================================
      SUBROUTINE DDSPLIT_COUNT_BC_LOADS(
     1   P           ,NCONLD      ,IBCL        ,CEP         ,
     2   NFXVEL      ,IBFV        ,DD_LAGF     ,NLINK       ,
     3   NNLINK      ,LLLINK      ,NGRAV       ,IGRAV       ,
     4   LGRAV       ,NODLOCAL    ,NUMNOD_L    ,NIBVEL      ,
     5   IBVEL       ,LBVEL       ,NCONLD_L    ,NFXVEL_L    ,
     6   NLLINK_L    ,NSKYLL_L    ,LLGRAV_L    ,LLBVEL_L    ,
     7   NSKYRBM_L   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: P, NCONLD, NFXVEL, NLINK, NGRAV, NIBVEL
      INTEGER, INTENT(IN) :: CEP(*), IBCL(*), IBFV(*), DD_LAGF(*)
      INTEGER, INTENT(IN) :: NNLINK(*), LLLINK(*), IGRAV(*), LGRAV(*)
      INTEGER, INTENT(IN) :: NODLOCAL(*), IBVEL(*), LBVEL(*)
      INTEGER, INTENT(IN) :: NUMNOD_L
      INTEGER, INTENT(OUT) :: NCONLD_L, NFXVEL_L, NLLINK_L, NSKYLL_L
      INTEGER, INTENT(OUT) :: LLGRAV_L, LLBVEL_L, NSKYRBM_L
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      EXTERNAL C_IBCL, C_IBFV, C_LLINK, C_IGRAV, C_IBVEL
C-----------------------------------------------
C--------------------------------------
C counts IBCL
C--------------------------------------
      NCONLD_L = 0
      IF (NCONLD>0)
     .  CALL C_IBCL(IBCL,CEP,P-1,NCONLD_L)
C--------------------------------------
C counts Fixvel
C--------------------------------------
      NFXVEL_L = 0
      IF (NFXVEL>0)
     .  CALL C_IBFV(IBFV,NFXVEL,NFXVEL_L,P-1,DD_LAGF)
C--------------------------------------
C counts LLINK
C--------------------------------------
      NLLINK_L=0
      NSKYLL_L=0
      IF (NLINK>0)
     .  CALL C_LLINK(NNLINK,LLLINK,P-1,NLLINK_L,NSKYLL_L)
C--------------------------------------
C counts LLGRAV
C--------------------------------------
      LLGRAV_L=0
      IF (NGRAV>0)
     .  CALL C_IGRAV(IGRAV,LGRAV,LLGRAV_L,P-1,NODLOCAL,NUMNOD_L)
C--------------------------------------
C counts IBVEL
C--------------------------------------
      LLBVEL_L=0
      NSKYRBM_L = 0
      IF (NIBVEL>0)
     .  CALL C_IBVEL(IBVEL,LBVEL,LLBVEL_L,NSKYRBM_L,P-1)
C-----------------------------------------------
      RETURN
      END SUBROUTINE DDSPLIT_COUNT_BC_LOADS
