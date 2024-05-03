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
      !||    dsgraph_mod   ../starter/share/modules1/dsgraph_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit       ../starter/source/restart/ddsplit/ddsplit.F
      !||    dswrest       ../starter/source/restart/ddsplit/wrrest.F
      !||    dswrestp      ../starter/source/restart/ddsplit/wrrest.F
      !||    lectur        ../starter/source/starter/lectur.F
      !||====================================================================
      MODULE DSGRAPH_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE SUPEL
         INTEGER :: NDDL_I, NDDL_F, NSDMAX,
     .              NDDL_M, NMDYN, KREC, MREC, MDREC, KIREC, 
     .              NDDL_I_INI, NDDL_F_INI
         INTEGER, DIMENSION(:), POINTER :: DDLS, DDLS_INI, CHILD, CMDYN,
     .                                     IFACM, IFACM_INI
         INTEGER, DIMENSION(:,:), POINTER :: IFAC, IFAC_INI
         my_real
     .          , DIMENSION(:,:), POINTER :: B, IFI, D, DM
      END TYPE SUPEL
C      
      TYPE PRGRAPH
         INTEGER :: NDDL, NDDL_INI, NSUPEL, IFILE, NSLEVEL, NSDEC,
     .              NSVMAX, IPRI, NDDL_GLOB, NNRP, NNCP, NDDLC
         my_real
     .           :: CUTFREQ
         INTEGER, DIMENSION(:,:), POINTER :: LSDDL, LSDDL_INI,
     .                                       IPROW, IPCOL
         INTEGER, DIMENSION(:), POINTER :: LSDDL_GLOB, LSDDL_GLOB_INI
         TYPE(SUPEL), DIMENSION(:), POINTER :: DGRAPH
      END TYPE PRGRAPH
C
      END MODULE DSGRAPH_MOD
      
