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
      !||    read_db             ../starter/source/restart/ddsplit/wrrest.F
      !||--- called by ------------------------------------------------------
      !||    intstamp_rrestr     ../starter/share/modules1/intstamp_mod.F
      !||    lec_inimap1d_file   ../starter/source/initial_conditions/inimap/lec_inimap1d_file.F
      !||    lec_inimap2d_file   ../starter/source/initial_conditions/inimap/lec_inimap2d_file.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE READ_DB(A,N)
C lecture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
      IF(ICRAY==1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
       DO I = 1, N1
         CALL READ_DB_C(R4,1000)
         DO J = 1, 1000
           K=K+1
           A(K) = R4(J)
         ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
         CALL READ_DB_C(R4,N-N2)
         DO J = 1, N-N2
           K=K+1
           A(K) = R4(J)
         ENDDO
       ENDIF
      ELSE
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        CALL READ_DB_C(R8,1000)
        DO J = 1, 1000
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        CALL READ_DB_C(R8,N-N2)
        DO J = 1, N-N2
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDIF
      ENDIF
C
      RETURN
      END

      !||====================================================================
      !||    read_dpdb         ../starter/source/restart/ddsplit/wrrest.F
      !||--- called by ------------------------------------------------------
      !||    intstamp_rrestr   ../starter/share/modules1/intstamp_mod.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE READ_DPDB(A,N)
C lecture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
      DOUBLE PRECISION A(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        CALL READ_DB_C(R8,1000)
        DO J = 1, 1000
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        CALL READ_DB_C(R8,N-N2)
        DO J = 1, N-N2
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDIF
C
      RETURN
      END
