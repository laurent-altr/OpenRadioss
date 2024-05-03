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
      !||    skew_mod          ../starter/share/modules1/skew_mod.F
      !||--- called by ------------------------------------------------------
      !||    check_skew        ../starter/source/spmd/domain_decomposition/check_skew.F
      !||    ddsplit           ../starter/source/restart/ddsplit/ddsplit.F
      !||    domdec2           ../starter/source/spmd/domdec2.F
      !||    lectur            ../starter/source/starter/lectur.F
      !||    split_skew        ../starter/source/restart/ddsplit/split_skew.F
      !||    split_skew_save   ../starter/source/restart/ddsplit/split_skew.F
      !||====================================================================
        MODULE SKEW_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
        TYPE SKEW_TYPE
!--------------------------------------------------------------------
!       MULTIPLE_SKEW : SKEW_TYPE ; dimension=NUMSKW+1
!                       MULTIPLE_SKEW(I)%PLIST(:) is a list of processor
!                       where the SKEW is stuck
!--------------------------------------------------------------------
                INTEGER, DIMENSION(:), ALLOCATABLE :: PLIST  
        END TYPE SKEW_TYPE
C
        END MODULE SKEW_MOD
