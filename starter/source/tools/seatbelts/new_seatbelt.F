Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
      MODULE NEW_SEATBELT_MOD
!//Encapsulating each routine in a module should be made mandatory
!// - in order to follow Fortran 90 standard when explicit interfaces are required 
!// - in order to have more checks at compile time, and decrease the need for static analysis tools
!//     (here, Radioss will NOT compile, because one caller of this subroutines is passing scalars
!//      instead of arrays. Argument that are not used should be made optional, instead of passing 
!//      variables of the wrong type/dimension)
      CONTAINS
!> @brief This routine does something 
!!
!! Here is a longer description of the purpose of the routine                                                                             
!! Do not hesitate to provide details.
!! A better name could be coined: this routine does not create/allocate new seatblets
!! but rather tag the nodes that belong to a given seatbelt with the seatblet ID
!!
Chd|====================================================================
Chd|  NEW_SEATBELT                  source/tools/seatbelts/new_seatbelt.F
Chd|-- called by -----------
Chd|        CREATE_SEATBELT               source/tools/seatbelts/create_seatbelt.F
Chd|        INI_SEATBELT                  source/tools/seatbelts/ini_seatbelt.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE NEW_SEATBELT(
     1   IXR,       ITAB,      KNOD2EL1D, NOD2EL1D,
     2   NOD_START, ELEM_CUR,  TAG_RES,   TAG_NOD,
     3   ID,        FLAG,      NNOD,      IPM,
     4   NB_ELEM_1D,NB_BRANCH, BRANCH_TAB,BRANCH_CPT,
     5   NUMELT,    NUMELP,    NUMELR,    NUMMAT,
     6   SIXX,      NUMNOD,    NRETRACTOR,NSLIPRING,
     7   NPROPMI)
C// The routine may be renamed TAG_SEATBELT_NODES
C// The number of Argument per line is 4, the maximum length of the line should be 72 (depending on the Doxygen version) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD, only : ANCMSG,ANINFO,MSGERROR,MSGWARNING
C// The SEATBELT module should contain only definition of types, here it contains also global variables
C// RETRACTOR should be instatiated in resol.F and passed by argument
      USE SEATBELT_MOD, only : RETRACTOR
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C// Note that there is no common blocks, everything is passed by argument
C// This will increase the number of arguments but: check bounds ifort/ifx option will work
C// A better solution would be to use assumed-shape arrays (but would require more work)
C// Using global variables for variables that does not change during the run (number of nodes etc)
C// may be acceptable if those variables are placed in a module and use,only is used
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C// Naming convention
C// - Couples of syllabes separated by underscore
C// - No variable with the name as a Fortran Keyword 
C// - INT for INTeger, INTER for INTERface, INTEG for INTEGration
C// - N_ prefix for the number of entity (use NODE for nodes)
C// - S_ prefix for the size of the array
C// - No French in variable names (SPRING instead of RES for RESsort)
C// - Avoid using generic terms such as ID, FLAG ... when the object it refers to is not obvious within the current file
      INTEGER, INTENT(IN) :: NPROPMI  !< number of properties 
      INTEGER, INTENT(IN) :: NUMNOD   !< number of nodes
      INTEGER, INTENT(IN) :: NRETRACTOR !< number of retractors (need to choose between NOBJECT, NUMOBJECT, NUM_OBJECT for the number of objects)                                         
      INTEGER, INTENT(IN) :: NSLIPRING  !< number of sliprings                                                    
      INTEGER, INTENT(IN) :: NUMELR !< number of springs
      INTEGER, INTENT(IN) :: NUMELT !< number of (Triangle, Truss)? 
      INTEGER, INTENT(IN) :: NUMMAT !< number of materials
      INTEGER, INTENT(IN) :: NUMELP !< number of beams 
      INTEGER, INTENT(IN) :: SIXX !< number of elemX
      INTEGER, INTENT(IN) :: IXR(NIXR,NUMELR) !< nodal conectivity of springs          
      INTEGER, INTENT(IN) :: ITAB(NUMNOD)     !< nodes user id   
      INTEGER, INTENT(IN) :: KNOD2EL1D(NUMNOD+1)  !< bounds for NOD2EL1D  
      INTEGER, INTENT(IN) :: NOD2EL1D(2*NUMELT+2*NUMELP+3*NUMELR+2*SIXX) !< NOD2EL1D(KNOD2EL1D(i):KNOD2EL1D(i+1)-1) : list of nodes of the ith 1D element
      INTEGER, INTENT(IN) :: NOD_START !< starting node 
      INTEGER, INTENT(IN) :: ID  !< id of something          
      INTEGER, INTENT(IN) :: FLAG  !< a flag that might mean something
      INTEGER, INTENT(IN) :: IPM(NPROPMI,NUMMAT) !< the infamous IPM         
      INTEGER, INTENT(IN) :: NB_ELEM_1D  !< number of 1D elements
      INTEGER, INTENT(INOUT) :: ELEM_CUR  !< current element   
      INTEGER, INTENT(INOUT) :: NNOD !< the number of nodes of something       
      INTEGER, INTENT(INOUT) :: BRANCH_TAB(2*NB_ELEM_1D) !< the branch table
      INTEGER, INTENT(INOUT) :: BRANCH_CPT  !< the branch ComPTeur
      INTEGER, INTENT(INOUT) :: NB_BRANCH   !< the number of branches: how is it different from BRANCH_CPT? 
      INTEGER, INTENT(INOUT) :: TAG_RES(NUMELR) !< is res result or "ressort" ?      
      INTEGER, INTENT(INOUT) :: TAG_NOD(NUMNOD) !< SPLIPRING_ID ?  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,NODE_CUR,NRES_FOUND,ELEM_NEWT,NODE_NEXT,ELEM_NEXT,ID_PREV,MTYP,MID,ELEM_TEST
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
 
C     Loop to find elements of the seatbelt
 
      NODE_CUR = NOD_START
      ELEM_NEXT = 0
      IF (FLAG == 0) TAG_RES(ELEM_CUR) = ID
      TAG_NOD(IXR(2,ELEM_CUR)) = ID
      TAG_NOD(IXR(3,ELEM_CUR)) = ID
      NNOD = NNOD + 1
      NRES_FOUND = 1
 
      DO WHILE (NRES_FOUND > 0)
        NRES_FOUND = 0
 
        IF (IXR(2,ELEM_CUR) == NODE_CUR) THEN
          NODE_NEXT = IXR(3,ELEM_CUR)
        ELSE
          NODE_NEXT = IXR(2,ELEM_CUR)
        ENDIF
 
        DO K=KNOD2EL1D(NODE_NEXT)+1,KNOD2EL1D(NODE_NEXT+1)
          IF ((NOD2EL1D(K) > NUMELT+NUMELP).AND.(NOD2EL1D(K) /= ELEM_CUR+NUMELT+NUMELP)) THEN
            ELEM_TEST = NOD2EL1D(K)-NUMELT-NUMELP
            MID = IXR(5,ELEM_TEST)
            IF (MID > 0) THEN
              MTYP = IPM(2,MID)
              IF ((MTYP == 114).AND.(TAG_RES(ELEM_TEST) == 0)) THEN
                NRES_FOUND = NRES_FOUND + 1
                IF(NRES_FOUND > 1) THEN
                  IF (NSLIPRING+NRETRACTOR > 0) THEN
C                   in case of sliprings and retractors bifurcation is not allowed
                    CALL ANCMSG(MSGID=2005,
     .                         MSGTYPE=MSGERROR,
     .                         ANMODE=ANINFO,
     .                         I1=ITAB(NODE_NEXT))
                    NRES_FOUND = 0
                  ELSE
C                   start of the secondary branch is saved
                    NB_BRANCH = NB_BRANCH + 1
                    BRANCH_CPT = BRANCH_CPT + 1
                    CALL ANCMSG(MSGID=2098,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO,
     .                          I1=ITAB(NODE_NEXT))
                    BRANCH_TAB(2*(BRANCH_CPT-1)+1) = NODE_NEXT
                    BRANCH_TAB(2*(BRANCH_CPT-1)+2) = ELEM_TEST
                  ENDIF
                ELSE
                  ELEM_NEXT = ELEM_TEST
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
 
        IF(NRES_FOUND > 0) THEN
          IF (FLAG == 0) THEN
            TAG_RES(ELEM_NEXT) = ID
            TAG_NOD(IXR(2,ELEM_NEXT)) = ID
            TAG_NOD(IXR(3,ELEM_NEXT)) = ID
            NNOD = NNOD + 1
          ELSE
            IF(TAG_RES(ELEM_NEXT) > 0) THEN
              ID_PREV = RETRACTOR(TAG_RES(ELEM_NEXT))%ID
              IF((ID_PREV > 0).AND.(NRES_FOUND > 0)) CALL ANCMSG(MSGID=2010,
     .           MSGTYPE=MSGERROR,
     .           ANMODE=ANINFO,
     .           I1=ID_PREV,I2=IXR(NIXR,ELEM_NEXT),I3=RETRACTOR(ID)%ID)
            ENDIF
            TAG_RES(ELEM_NEXT) = ID
            TAG_NOD(IXR(2,ELEM_NEXT)) = ID
            TAG_NOD(IXR(3,ELEM_NEXT)) = ID
          ENDIF
        ENDIF

        ELEM_CUR = ELEM_NEXT
        NODE_CUR = NODE_NEXT
      ENDDO
 
      END SUBROUTINE NEW_SEATBELT
      END MODULE
