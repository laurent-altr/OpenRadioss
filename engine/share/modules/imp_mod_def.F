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
      !||    imp_intbufdef    ../engine/share/modules/imp_mod_def.F
      !||--- called by ------------------------------------------------------
      !||    i24ke3           ../engine/source/interfaces/int24/i24ke3.F
      !||    imp_intbuf       ../engine/share/modules/imp_mod_def.F
      !||    imp_intbuf_ini   ../engine/source/implicit/imp_solv.F
      !||====================================================================
      MODULE IMP_INTBUFDEF 
C----------------------------------------------------------------------- 
#include "my_real.inc"      
c
C=======================================================================
      TYPE IMP_INTBUF_STRUCT_
C=================================================  
c define typeINTBUF_STRUCT_ for interface buffer IMP_INTBUF_TAB
C=======================================================================
c DEFINE SIZES (integers arrays)
C=======================================================================
        integer ::   S_I_STOK     !  replace NUM_IMP (after)            
        integer ::   S_CAND_N     !  replace NS_IMP (after)  
        integer ::   S_CAND_E     !  replace NE_IMP (after)   
        integer ::   S_INDSUBT    !  replace IND_IMP (after)  
C=======================================================================
c DEFINE SIZES (float arrays)
C=======================================================================
        integer ::   S_HJ           !  (4,S_I_STOK)
        integer ::   S_NJ           !  (3,S_I_STOK)       Normal
        integer ::   S_STIF         !  (3,S_I_STOK)       N
C=======================================================================
c DEFINE ARRAYS (integers arrays)
C=======================================================================
        integer, DIMENSION(:) , POINTER ::  I_STOK      
        integer, DIMENSION(:) , POINTER ::  CAND_N      
        integer, DIMENSION(:) , POINTER ::  CAND_E    
        integer, DIMENSION(:) , POINTER ::  INDSUBT   
!type24
C=======================================================================
c DEFINE ARRAYS (float arrays)
C=======================================================================
        my_real, DIMENSION(:) , POINTER ::   HJ           
        my_real, DIMENSION(:) , POINTER ::   NJ           
        my_real, DIMENSION(:) , POINTER ::   STIF         
C=======================================================================
      END TYPE IMP_INTBUF_STRUCT_
C=======================================================================
c---------------
      END MODULE IMP_INTBUFDEF
      !||====================================================================
      !||    imp_intbuf       ../engine/share/modules/imp_mod_def.F
      !||--- called by ------------------------------------------------------
      !||    cp_impbuf        ../engine/source/implicit/produt_v.F
      !||    deallocm_imp     ../engine/source/implicit/imp_solv.F
      !||    dim_int7         ../engine/source/implicit/ind_glob_k.F
      !||    dim_int_k        ../engine/source/implicit/ind_glob_k.F
      !||    dim_kine_i       ../engine/source/implicit/ind_glob_k.F
      !||    i24main_tri      ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24mainf         ../engine/source/interfaces/int24/i24main.F
      !||    i25main_tri      ../engine/source/interfaces/intsort/i25main_tri.F
      !||    imp_int_k        ../engine/source/implicit/imp_int_k.F
      !||    ind_int_k        ../engine/source/implicit/ind_glob_k.F
      !||    ini_bminma_imp   ../engine/source/implicit/imp_solv.F
      !||    resol            ../engine/source/engine/resol.F
      !||--- uses       -----------------------------------------------------
      !||    imp_intbufdef    ../engine/share/modules/imp_mod_def.F
      !||    intbufdef_mod    ../common_source/modules/intbufdef_mod.F
      !||====================================================================
      MODULE IMP_INTBUF
        USE INTBUFDEF_MOD
        USE IMP_INTBUFDEF
        TYPE(INTBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: INTBUF_TAB_CP
        TYPE(IMP_INTBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: INTBUF_TAB_IMP
      my_real, DIMENSION(:,:), ALLOCATABLE :: BMINMA_IMP
      END MODULE IMP_INTBUF
      !||====================================================================
      !||    imp_inttd    ../engine/share/modules/imp_mod_def.F
      !||--- called by ------------------------------------------------------
      !||    cp_inttd     ../engine/source/implicit/imp_int_k.F
      !||    imp_int_k    ../engine/source/implicit/imp_int_k.F
      !||    imp_rnumcd   ../engine/source/implicit/imp_int_k.F
      !||    sav_inttd    ../engine/source/implicit/imp_int_k.F
      !||====================================================================
      MODULE IMP_INTTD
C tableau de travail interface traverse detection 
      INTEGER, DIMENSION(:),ALLOCATABLE :: NS_IMP1,IAD1_NIN
      INTEGER, DIMENSION(:),ALLOCATABLE :: NE_IMP1,IND_IMP1
C      
      END MODULE IMP_INTTD
C  52f4 ---
