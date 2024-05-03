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
      !||    visc_param_mod         ../common_source/modules/mat_elem/visc_param_mod.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_visc_lprony    ../starter/source/materials/visc/hm_read_visc_lprony.F
      !||    hm_read_visc_prony     ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    matparam_def_mod       ../common_source/modules/mat_elem/matparam_def_mod.F
      !||    read_viscparam         ../engine/source/output/restart/read_viscparam.F
      !||    updmat                 ../starter/source/materials/updmat.F
      !||    visc_et                ../engine/source/elements/solid/solidez/visc_et.F
      !||    visc_prony             ../engine/source/materials/visc/visc_prony.F
      !||    visc_prony_lstrain     ../engine/source/materials/visc/visc_prony_lstrain.F
      !||    viscmain               ../engine/source/materials/visc/viscmain.F
      !||    write_viscparam        ../engine/source/output/restart/write_viscparam.F
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||    table4d_mod            ../common_source/modules/table4d_mod.F
      !||====================================================================
      MODULE VISC_PARAM_MOD
      USE TABLE4D_MOD
      USE NAMES_AND_TITLES_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c defines data structure for viscosity model parameters
c=======================================================================      
      
      TYPE VISC_PARAM_
        INTEGER     :: ILAW                   ! viscosity model type (number)
        CHARACTER(LEN=NCHARTITLE) :: TITLE    ! viscosity model input name
        INTEGER     :: NUPARAM                ! number of real value material paraameters
        INTEGER     :: NIPARAM                ! number of int value material parameters
        INTEGER     :: NUVAR                  ! number of internal state variables
        INTEGER     :: NFUNC                  ! number of local functions in material
        INTEGER     :: NTABLE                 ! number of local function tables
        
        my_real        ,DIMENSION(:) ,ALLOCATABLE :: UPARAM  ! NUPARAM
        INTEGER        ,DIMENSION(:) ,ALLOCATABLE :: IPARAM  ! NIPARAM
        INTEGER        ,DIMENSION(:) ,ALLOCATABLE :: FUNC    ! NFUNC
        TYPE(TABLE_4D_),DIMENSION(:) ,ALLOCATABLE :: TABLE   ! NTABLE
      
      END TYPE VISC_PARAM_   
c
c---------------
      END MODULE VISC_PARAM_MOD
