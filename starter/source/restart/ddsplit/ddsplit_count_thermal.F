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
!||    ddsplit_count_thermal            ../starter/source/restart/ddsplit/ddsplit_count_thermal.F
!||--- called by ------------------------------------------------------
!||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    c_iconv                          ../starter/source/restart/ddsplit/c_iconv.F
!||    c_iradia                         ../starter/source/restart/ddsplit/c_iradia.F
!||    c_ithflux                        ../starter/source/restart/ddsplit/c_ithflux.F
!||    c_ibft                           ../starter/source/restart/ddsplit/c_ibft.F
!||====================================================================
      SUBROUTINE DDSPLIT_COUNT_THERMAL(
     1   P           ,CEP         ,GLOB_THERM  ,IBCV        ,
     2   IBCR        ,IBFFLUX     ,IBFT        ,DD_LAGF     ,
     3   NCONV_L     ,NRADIA_L    ,NFXFLUX_L   ,NFXTEMP_L   )
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
      INTEGER, INTENT(IN) :: P
      INTEGER, INTENT(IN) :: CEP(*), IBCV(*), IBCR(*), IBFFLUX(*)
      INTEGER, INTENT(IN) :: IBFT(*), DD_LAGF(*)
      TYPE(glob_therm_), INTENT(IN) :: GLOB_THERM
      INTEGER, INTENT(OUT) :: NCONV_L, NRADIA_L, NFXFLUX_L, NFXTEMP_L
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      EXTERNAL C_ICONV, C_IRADIA, C_ITHFLUX, C_IBFT
C-----------------------------------------------
C--------------------------------------
C Convec flow courses
C--------------------------------------
      NCONV_L = 0
      IF (GLOB_THERM%NUMCONV > 0)
     .    CALL C_ICONV(IBCV,CEP,P-1,NCONV_L,GLOB_THERM%NUMCONV,GLOB_THERM%NICONV)
C--------------------------------------
C counts radiative fluxes
C--------------------------------------
      NRADIA_L = 0
      IF (GLOB_THERM%NUMRADIA > 0) CALL C_IRADIA(IBCR,CEP,P-1,NRADIA_L,
     .           GLOB_THERM%NUMRADIA,GLOB_THERM%NUMCONV,GLOB_THERM%NIRADIA)
C--------------------------------------
C counts imposed fluxes
C--------------------------------------
      NFXFLUX_L = 0
      IF (GLOB_THERM%NFXFLUX > 0) CALL C_ITHFLUX(IBFFLUX,CEP,P-1,NFXFLUX_L,
     .           GLOB_THERM%NUMCONV,GLOB_THERM%NUMRADIA,GLOB_THERM%NFXFLUX,GLOB_THERM%NITFLUX)
C--------------------------------------
c counts Fixtemp
C--------------------------------------
      NFXTEMP_L = 0
      IF (GLOB_THERM%NFXTEMP > 0)
     .  CALL C_IBFT(IBFT,GLOB_THERM%NFXTEMP,NFXTEMP_L,P-1,DD_LAGF,GLOB_THERM%NIFT)
C-----------------------------------------------
      RETURN
      END SUBROUTINE DDSPLIT_COUNT_THERMAL
