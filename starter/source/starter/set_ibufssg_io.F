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
!||    set_ibufssg_io            ../starter/source/starter/set_ibufssg_io.F
!||--- called by ------------------------------------------------------
!||    lectur                    ../starter/source/starter/lectur.F
!||====================================================================
      SUBROUTINE SET_IBUFSSG_IO(ISPHIO, IGRSURF, IBUFSSG_IO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPHIO(NISPHIO,*),
     .        IBUFSSG_IO(SIBUFSSG_IO),
     .        N,J,NSEG,IN1,IN2,IN3,IN4,
     .        ISU,PROC,IAD2,ITYPE
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      IAD2 = 1

      DO N=1,NSPHIO
        ITYPE = ISPHIO(1,N)
        IF(ISPHIO(12,N)==0) THEN
          ISU = ISPHIO(3,N)
          NSEG= IGRSURF(ISU)%NSEG
          ISPHIO(10,N) = NSEG
          ISPHIO(11,N) = IAD2
          DO J=0,NSEG-1
            IN1=IGRSURF(ISU)%NODES(J+1,1)
            IN2=IGRSURF(ISU)%NODES(J+1,2)
            IN3=IGRSURF(ISU)%NODES(J+1,3)
            IN4=IGRSURF(ISU)%NODES(J+1,4)
            IBUFSSG_IO(IAD2+NIBSPH*J)   = IN1
            IBUFSSG_IO(IAD2+NIBSPH*J+1) = IN2
            IBUFSSG_IO(IAD2+NIBSPH*J+2) = IN3
            IBUFSSG_IO(IAD2+NIBSPH*J+3) = IN4
            DO PROC=1,NSPMD
              CALL IFRONTPLUS(IN1,PROC)
              CALL IFRONTPLUS(IN2,PROC)
              CALL IFRONTPLUS(IN3,PROC)
              CALL IFRONTPLUS(IN4,PROC)
            ENDDO
          ENDDO
          IAD2 = IAD2 + 4*NSEG
        ELSEIF(ISPHIO(12,N)==2) THEN
          IN1 = ISPHIO(13,N)
          IN2 = ISPHIO(14,N)
          IN3 = ISPHIO(15,N)
          DO PROC=1,NSPMD
            CALL IFRONTPLUS(IN1,PROC)
            CALL IFRONTPLUS(IN2,PROC)
            CALL IFRONTPLUS(IN3,PROC)
          ENDDO
        ENDIF
      ENDDO
C=======================================================================
      RETURN
      END
