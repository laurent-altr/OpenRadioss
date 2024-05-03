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
      !||    int8_mod           ../common_source/modules/interfaces/int8_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit            ../starter/source/restart/ddsplit/ddsplit.F
      !||    i8loc3             ../engine/source/interfaces/inter3d/i8loc3.F
      !||    i8msr3             ../engine/source/interfaces/inter3d/i8msr3.F
      !||    intbuf_ini_t8      ../common_source/interf/intbuf_ini.F
      !||    intbuf_mod         ../engine/share/modules/restart_mod.F
      !||    intbufdef_mod      ../common_source/modules/intbufdef_mod.F
      !||    intfop8            ../engine/source/interfaces/interf/intfop8.F
      !||    intvo8             ../engine/source/interfaces/inter3d/intvo8.F
      !||    lectur             ../engine/source/input/lectur.F
      !||    prepare_split_i8   ../starter/source/restart/ddsplit/inter_tools.F
      !||    set_front8         ../starter/source/spmd/node/ddtools.F
      !||    split_interfaces   ../starter/source/restart/ddsplit/split_interfaces.F
      !||    spmd_i8_commslv    ../engine/source/mpi/interfaces/spmd_i8tool.F
      !||    spmd_i8_iloc       ../engine/source/mpi/interfaces/spmd_i8tool.F
      !||    spmd_i8_index      ../engine/source/mpi/interfaces/spmd_i8tool.F
      !||    spmd_i8_irtl       ../engine/source/mpi/interfaces/spmd_i8tool.F
      !||    spmd_i8_updbuf     ../engine/source/mpi/interfaces/spmd_i8tool.F
      !||    w_type8            ../starter/source/restart/ddsplit/split_interfaces.F
      !||    write_intbuf_t8    ../engine/source/output/restart/write_intbuf.F
      !||    wrrestp            ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE INT8_MOD 
C----------------------------------------------------------------------- 
#include "my_real.inc"      

      TYPE BUFT8
        integer ::  NBMAIN
        integer ::  NBSECND_TOT
        integer, DIMENSION(:) , POINTER ::  NBSECND !table of nbsecnd per main
        integer, DIMENSION(:) , POINTER ::  MAIN_UID
        integer, DIMENSION(:) , POINTER ::  MAIN_ID
        integer, DIMENSION(:) , POINTER ::  SECND_UID
        integer, DIMENSION(:) , POINTER ::  SECND_ID
C Send buffer for I8MSR3
C       my_real, DIMENSION(:) , POINTER ::  BUFR 
C       integer, DIMENSION(:) , POINTER ::  BUFI 
C Send Buffer for I8LOC3
C       my_real, DIMENSION(:) , POINTER :: DISTANCE 
C       integer, DIMENSION(:) , POINTER ::  NEW_MAIN_UID

      END TYPE BUFT8 
      TYPE FRONT8
        !Local number of the main node :
        integer ::  NUMLOC  
        !User ID of the main node :
        integer ::  UID
        ! number of time the interface main has to be
        ! sent and received                                
        integer  ::  NBCOM !  
        !List (of size NBCOM) of processors that share the main
        integer, DIMENSION(:) , POINTER ::  PROCLIST  
        !Pointer to the structure BUFFER        
        integer, DIMENSION(:) , POINTER ::  BUF_INDEX  
      END TYPE FRONT8 

      TYPE INT8_STRUCT_ 
        integer :: NI
        integer :: S_COMM 
        integer :: IS_ACTIVATED
        TYPE(BUFT8), DIMENSION(:) , POINTER :: BUFFER  
        TYPE(FRONT8), DIMENSION(:) , POINTER :: SPMD_COMM_PATTERN 
      END TYPE

      END MODULE
