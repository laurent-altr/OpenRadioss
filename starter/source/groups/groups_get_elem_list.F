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
      !||    groups_get_elem_list   ../starter/source/groups/hm_tagpart.F
      !||--- called by ------------------------------------------------------
      !||    groups_get_elem_list   ../starter/source/groups/hm_tagpart.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/share/message_module/inter_ancmsg_mod.F
      !||    groups_get_elem_list   ../starter/source/groups/hm_tagpart.F
      !||--- uses       -----------------------------------------------------
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE GROUPS_GET_ELEM_LIST(LIST_ENTITY,NENTITY,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: NENTITY
      INTEGER,INTENT(INOUT),ALLOCATABLE,DIMENSION(:) :: LIST_ENTITY
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NENTITY_POS,NENTITY_NEG,JJ,KK
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C CONSTITUTION DE LIST
C-----------------------------------------------
        CALL HM_GET_INTV  ('idsmax' ,NENTITY_POS,IS_AVAILABLE,LSUBMODEL)                 
        CALL HM_GET_INTV  ('negativeIdsmax' ,NENTITY_NEG,IS_AVAILABLE,LSUBMODEL)
        NENTITY = NENTITY_POS + NENTITY_NEG 
        ALLOCATE(LIST_ENTITY(NENTITY))    
        DO KK=1,NENTITY_POS                                                             
         CALL HM_GET_INT_ARRAY_INDEX ('ids' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)  
         LIST_ENTITY(KK)=JJ
        ENDDO  
        DO KK=1,NENTITY_NEG                                                            
         CALL HM_GET_INT_ARRAY_INDEX ('negativeIds' ,JJ ,KK,IS_AVAILABLE,LSUBMODEL)  
         LIST_ENTITY(NENTITY_POS+KK)=-JJ
        ENDDO        
C-----------------------

      RETURN
      END     
