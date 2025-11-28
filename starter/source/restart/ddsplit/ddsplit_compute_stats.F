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
!||    ddsplit_compute_stats            ../starter/source/restart/ddsplit/ddsplit_compute_stats.F
!||--- called by ------------------------------------------------------
!||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
!||====================================================================
      SUBROUTINE DDSPLIT_COMPUTE_STATS(
     1   DDSTAT      ,NUMNOD_L    ,NUMEL_L     ,NUMELS_L    ,
     2   NUMELQ_L    ,NUMELC_L    ,NUMELT_L    ,NUMELP_L    ,
     3   NUMELR_L    ,NUMELTG_L   ,NUMELX_L    ,NBDDPROC    ,
     4   NBDDBOUN    ,NBDDNOD     ,NBDDNRB     ,NRBYKIN_L   ,
     5   NUMSPH_L    ,NSNT_L      ,NMNT_L      ,NSNT2_L     ,
     6   NMNT2_L     ,NSLARB_L    ,LEN_IA      ,LEN_AM      ,
     7   IPARI0      ,NISKY0_L    ,NCONV_L     ,NRADIA_L    ,
     8   NFXFLUX_L   ,IALE        ,IEULER      ,IALELAG     ,
     9   IRODDL      ,LENWA_L     ,NFUNCT      ,PROBINT     ,
     A   IMAXIMP_L   ,NSKYI18_L   ,NPSAV       ,NPART       ,
     B   LCNE_L      ,N2D         ,GLOB_THERM  ,LCNI2_L     ,
     C   NSKYRW_L    ,NSKYRBK_L   ,NISKYI2_L   ,NNMV_L      ,
     D   NNMVC_L     ,NSKYLL_L    ,NSKYRBM_L   ,NSKYRBE3_L  ,
     E   IVECTOR     ,I2NSNT      ,LWASPH_L    ,NSECT       ,
     F   ISECUT      ,NINTER      ,NIBVEL      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GLOB_THERM_MOD , only : glob_therm_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(OUT) :: DDSTAT(*)
      INTEGER, INTENT(IN) :: NUMNOD_L, NUMEL_L, NUMELS_L, NUMELQ_L
      INTEGER, INTENT(IN) :: NUMELC_L, NUMELT_L, NUMELP_L, NUMELR_L
      INTEGER, INTENT(IN) :: NUMELTG_L, NUMELX_L, NBDDPROC, NBDDBOUN
      INTEGER, INTENT(IN) :: NBDDNOD, NBDDNRB, NRBYKIN_L, NUMSPH_L
      INTEGER, INTENT(IN) :: NSNT_L, NMNT_L, NSNT2_L, NMNT2_L
      INTEGER, INTENT(IN) :: NSLARB_L, LEN_IA, LEN_AM
      INTEGER, INTENT(IN) :: IPARI0, NCONV_L, NRADIA_L, NFXFLUX_L
      INTEGER, INTENT(IN) :: IALE, IEULER, IALELAG, IRODDL
      INTEGER, INTENT(IN) :: LENWA_L, NFUNCT, IMAXIMP_L, NSKYI18_L
      INTEGER, INTENT(IN) :: NPSAV, NPART, LCNE_L, N2D, LCNI2_L
      INTEGER, INTENT(IN) :: NSKYRW_L, NSKYRBK_L, NISKYI2_L, NNMV_L
      INTEGER, INTENT(IN) :: NNMVC_L, NSKYLL_L, NSKYRBM_L, NSKYRBE3_L
      INTEGER, INTENT(IN) :: IVECTOR, I2NSNT, LWASPH_L, NSECT
      INTEGER, INTENT(IN) :: ISECUT, NINTER, NIBVEL
      INTEGER, INTENT(INOUT) :: NISKY0_L
      my_real, INTENT(IN) :: PROBINT
      TYPE(glob_therm_), INTENT(IN) :: GLOB_THERM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I_EVAL, R_EVAL, LSKYI
C-----------------------------------------------
C--------------------------------------------
C Stats Domain Decomp
C--------------------------------------------
C DDSTAT
C 1 : NUMNOD Local
C 2 : NELEM Local
C 3 : NUMELS_L
C 4 : NUMELQ_L
C 5 : NUMELC_L
C 6 : NUMELP_L
C 7 : NUMELT_L
C 8 : NUMELR_L
C 9 : -
C 10: NUMELTG_L
C 11: NUMELX_L
C 12: NBDDPROC :  nb of boundary proc 
C 13: NBDDBOUN :  nb of boundary node 
C 14: NBDDNOD :   size of comm:nb of nodes
C 15: NBDDNRB :   size of comm: nb of nodes main of rby
C 16: NRBYKIN_L : number of local rigid body main
C 17: NUMSPH_L : number of particles SPH (local) 
C 18: MEMI : size memory local int MA
C 19: MEMR : size memory local float AM
C 20: NSNT_L : number of nodes seconds (contact interface 7,10,11)
C 21: NMNT_L : number of nodes main (contact interface 7,10,11)
C 22: NSNT2_L : number of nodes seconds (interface type2)
C 23: NMNT2_L : number of nodes main (interface type2)
C 24: RESTSIZE : restart size in KB
C 24: NSLARB_L : number of nodes second rigid body
      DDSTAT(1) = NUMNOD_L
      DDSTAT(2) = NUMEL_L
      DDSTAT(3) = NUMELS_L
      DDSTAT(4) = NUMELQ_L
      DDSTAT(5) = NUMELC_L
      DDSTAT(6) = NUMELP_L
      DDSTAT(7) = NUMELT_L
      DDSTAT(8) = NUMELR_L
      DDSTAT(9) = 0
      DDSTAT(10)= NUMELTG_L
      DDSTAT(11)= NUMELX_L
      DDSTAT(12)= NBDDPROC
      DDSTAT(13)= NBDDBOUN
      DDSTAT(14)= NBDDNOD
      DDSTAT(15)= NBDDNRB
      DDSTAT(16)= NRBYKIN_L
      DDSTAT(17)= NUMSPH_L
      DDSTAT(20)= NSNT_L
      DDSTAT(21)= NMNT_L
      DDSTAT(22)= NSNT2_L
      DDSTAT(23)= NMNT2_L
      DDSTAT(24)= NSLARB_L
C
C Estimating memory usage
C
      IF(IPARI0/=0)THEN
        NISKY0_L=NISKY0_L+4*NCONV_L+4*NRADIA_L+4*NFXFLUX_L
      END IF
C
      I_EVAL = NUMNOD_L+2*NUMNOD_L*MAX(IALE,IEULER,IALELAG)+NUMNOD_L*IRODDL
      R_EVAL = LENWA_L+NFUNCT
      IF(IPARI0/=0)THEN
        LSKYI = NINT(5*MAX(4*NUMNOD_L,IMAXIMP_L)*PROBINT)+4*NUMSPH_L+NSKYI18_L
        R_EVAL = R_EVAL+8*NUMNOD_L+NPSAV*NPART
        R_EVAL = R_EVAL+8*LCNE_L
        IF (N2D==0.AND.IALE+IEULER+GLOB_THERM%ITHERM+IALELAG > 0) R_EVAL = R_EVAL+ LCNE_L
C NFSKYI = 5
        R_EVAL = R_EVAL+5*LSKYI
        I_EVAL = I_EVAL+LSKYI+NUMNOD_L+1+LCNI2_L
     +         + NISKY0_L+NSKYRW_L+NSKYRBK_L+NISKYI2_L
     +         + NNMV_L+NNMVC_L+NSKYLL_L+NSKYRBM_L+NSKYRBE3_L
     +         + NNMV_L+NNMVC_L+NSKYLL_L+NSKYRBM_L
        IF(IVECTOR==1)I_EVAL = I_EVAL+ NUMNOD_L
        IF(I2NSNT>0)I_EVAL = I_EVAL+ NUMNOD_L+1
        IF(I2NSNT>0.AND.IVECTOR==1)I_EVAL = I_EVAL+ NUMNOD_L
      ELSE
        R_EVAL = R_EVAL+7*NUMNOD_L+NUMNOD_L*IRODDL+NPSAV*NPART
        R_EVAL = R_EVAL+2*MAX(IALE,GLOB_THERM%ITHERM,IEULER)*(NUMELS_L+NUMELQ_L)
      END IF
C      IF(KDTINT/=0)
      R_EVAL = R_EVAL+NUMNOD_L
      R_EVAL = R_EVAL+
     +         LWASPH_L+7*NUMNOD_L*NSECT*ISECUT+5*NINTER+21*NIBVEL
      I_EVAL = I_EVAL+NUMNOD_L
      DDSTAT(18)= LEN_IA + I_EVAL
      DDSTAT(19)= LEN_AM + R_EVAL
C-----------------------------------------------
      RETURN
      END SUBROUTINE DDSPLIT_COMPUTE_STATS
