Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
!_______________________________________________________________________
!
! OpenRadioss                                            ____E_N_G_I_N_E
!_______________________________________________________________________

#ifdef ADAPTIVE_MPI
      SUBROUTINE MPI_Main
      IMPLICIT NONE
#include "mpif.h"
      INTEGER ISV_KEY
      PARAMETER (ISV_KEY=195118620)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER IERR,KEY,HINTS,NNODES,MYRANK
      KEY = ISV_KEY

      WRITE(6,*) __FILE__,__LINE__
      CALL FLUSH(6)

      CALL MPI_INITIALIZED(KEY,IERR)
      CALL MPI_INIT(IERR) 

      WRITE(6,*) __FILE__,__LINE__,MPI_COMM_WORLD
      CALL FLUSH(6)

      
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NNODES, IERR)

      WRITE(6,*) __FILE__,__LINE__
      CALL FLUSH(6)

      CALL MPI_COMM_RANK(MPI_COMM_WORLD, MYRANK, IERR)

      IF (MYRANK.EQ.0) CALL AMPI_SET_MIGRATABLE(0,IERR)

      CALL MPI_INFO_CREATE(HINTS,IERR)
      CALL MPI_INFO_SET(HINTS,"ampi_load_balance","sync",IERR)

      CALL RADIOSS0(HINTS)

      CALL MPI_INFO_FREE(HINTS,IERR)
      STOP
      END SUBROUTINE MPI_Main
#else
      PROGRAM RADIOSS

        CALL RADIOSS0()

      CALL MY_EXIT(0)
      END
#endif
!_______________________________________________________________________
