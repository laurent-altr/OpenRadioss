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
      !||    interfaces_mod         ../common_source/modules/interfaces/interfaces_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit                ../starter/source/restart/ddsplit/ddsplit.F
      !||    hm_read_inter_struct   ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_read_interfaces     ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_thgrou         ../starter/source/output/th/hm_read_thgrou.F
      !||    i24mainf               ../engine/source/interfaces/int24/i24main.F
      !||    i25mainf               ../engine/source/interfaces/int25/i25mainf.F
      !||    imp_inttd0             ../engine/source/implicit/imp_int_k.F
      !||    imp_solv               ../engine/source/implicit/imp_solv.F
      !||    imp_tripi              ../engine/source/implicit/imp_int_k.F
      !||    initia                 ../starter/source/elements/initia/initia.F
      !||    intfop2                ../engine/source/interfaces/interf/intfop2.F
      !||    inttri                 ../engine/source/interfaces/intsort/inttri.F
      !||    lectur                 ../engine/source/input/lectur.F
      !||    radioss2               ../engine/source/engine/radioss2.F
      !||    rdresa                 ../engine/source/output/restart/rdresa.F
      !||    rdresb                 ../engine/source/output/restart/rdresb.F
      !||    resol                  ../engine/source/engine/resol.F
      !||    resol_head             ../engine/source/engine/resol_head.F
      !||    spmd_i7fcom_poff       ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon        ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_i7xvcom2          ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    wrrestp                ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    parameters_mod         ../common_source/modules/interfaces/parameters_mod.F
      !||====================================================================
        MODULE INTERFACES_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE PARAMETERS_MOD
       implicit  none
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"

!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------

C----------------------------------------------
C   Global Interfaces stucture
C---------------------------------------------

          TYPE INTERFACES_
               TYPE (PARAMETERS_) PARAMETERS
          END TYPE INTERFACES_

C
        END MODULE INTERFACES_MOD
