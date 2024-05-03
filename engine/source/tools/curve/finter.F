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
#include "my_real.inc"
      !||====================================================================
      !||    finter   ../starter/source/tools/curve/finter.F
      !||====================================================================
        my_real FUNCTION FINTER(IFUNC,XX,NPF,TF,DERI)
!$COMMENT
!       FINTER description :
!              FINTER function interpolates XX on TF curve
!               and computes also the derivative DERI
!       FINTER organization :
!               two cases :
!                    - constant function --> direct interpolation
!                    - non-constant function :
!                        (1): if the number of point in the function is < MIN_GAP, then
!                             the interpolation is computed 
!                        (2): if the number of point in the function is > MIN_GAP, then
!                             a dichotmy reduces the point interval and a interpolation
!                             is performed on the reduced point interval
!                             if the dichotomy fails, then the classical interpolation 
!                             is used
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
!       ----------------------------------------
!       Global variables
!       ----------------------------------------
        INTEGER IFUNC,NPF(*)
        my_real TF(*),DERI,XX
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       IFUNC   : integer ; function number
!       NPF     : integer ; dimension=SUM( (number of point[IFUNC])*2), IFUNC=1:MAX_FUNC
!                 pointer for the function points x + function value f(x)
!       TF      : my_real ; dimension=SUM( (number of point[IFUNC])*2), IFUNC=1:MAX_FUNC
!                 gives the function points x + function value f(x)
!       DERI    : my_real ; function derivative
!       XX      : my_real ; x value to be interpolate 
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       ----------------------------------------
!       Local variables
!       ----------------------------------------
        LOGICAL :: BOOL
        INTEGER :: I,J,POINT_NBR
        INTEGER :: FIRST, LAST, MIDDLE
        INTEGER :: MIN_GAP
        INTEGER :: COUNTER
        my_real :: DX1,DX2,DX2_FIRST,DX2_LAST,DX2_MIDDLE
        my_real :: PRODUCT_FM,PRODUCT_ML
        my_real :: DIV, DIV0

!       ----------------------------------------
        MIN_GAP = 20
        DX2 = TF(NPF(IFUNC)) - XX
        !       -----------------------------
        IF ((NPF(IFUNC+1)-NPF(IFUNC))==2) THEN
        !       constant function
                FINTER = TF(NPF(IFUNC)+1)
                RETURN
        ELSE
        !       -----------------------------
        !       check the number of point in the function:
        !       if point_nbt < MIN_GAP --> classical interpolation
        !       if point_nbt > MIN_GAP --> dichotomy in order to reduce the point number interval
        !                            and then when the interval is < MIN_GAP --> classical interpolation
        !       -----------------------------
                POINT_NBR = ( NPF(IFUNC+1)-2 - (NPF(IFUNC)+2) ) / 2 + 1
                IF(POINT_NBR<MIN_GAP) THEN
                        !       -----------------------------
                        !       classical interpolation
                        DO I=NPF(IFUNC)+2,NPF(IFUNC+1)-2,2
                                DX1 = -DX2
                                DX2 = TF(I) - XX
                                IF(DX2>=ZERO.OR.I==NPF(IFUNC+1)-2)THEN
                                        DIV0 = TF(I) - TF(I-2)
                                        DIV = MAX(ABS(DIV0),EM16)
                                        DIV = SIGN(DIV,DIV0)
                                        DERI = (TF(I+1) - TF(I-1)) / DIV
                                        IF(DX1<=DX2)THEN
                                                FINTER = TF(I-1) + DX1 * DERI
                                        ELSE
                                                FINTER = TF(I+1) - DX2 * DERI
                                        ENDIF
                                        RETURN
                                ENDIF
                        ENDDO
                        !       -----------------------------
                ELSE
                        !       -----------------------------
                        !       dichotomy
                        !       first shot : (a) we check the first value 

                        DX2 = TF(NPF(IFUNC)) - XX
                        I=NPF(IFUNC)+2
                        DX1 = -DX2
                        DX2 = TF(I) - XX
                        IF(DX2>=ZERO.OR.I==NPF(IFUNC+1)-2)THEN
                                DIV0 = TF(I) - TF(I-2)
                                DIV = MAX(ABS(DIV0),EM16)
                                DIV = SIGN(DIV,DIV0)
                                DERI = (TF(I+1) - TF(I-1)) / DIV
                                IF(DX1<=DX2)THEN
                                        FINTER = TF(I-1) + DX1 * DERI
                                ELSE
                                        FINTER = TF(I+1) - DX2 * DERI 
                                ENDIF
                                RETURN
                        ENDIF

                        !       first shot : (b) we check the last value 
                        DX2 = TF(NPF(IFUNC+1)-2) - XX
                        I=NPF(IFUNC+1)-2
                        DX1 = -DX2
                        DX2 = TF(I) - XX
                        IF(DX2 <= ZERO)THEN
                                DIV0 = TF(I) - TF(I-2)
                                DIV = MAX(ABS(DIV0),EM16)
                                DIV = SIGN(DIV,DIV0)
                                DERI = (TF(I+1) - TF(I-1)) / DIV
                                IF(DX1 == ZERO .AND. DX2 == ZERO) THEN
                                  FINTER = TF(I+1)
                                ELSEIF(DX1<=DX2)THEN
                                        FINTER = TF(I-1) + DX1 * DERI
                                ELSE
                                        FINTER = TF(I+1) - DX2 * DERI
                                ENDIF
                                RETURN
                        ENDIF
                

                        !       -----------------------------
                        !       second shot : (a) dichotomy in order to reduce the gap
                        FIRST = 1 
                        LAST = POINT_NBR 
                        BOOL=.TRUE.
                        DX2 = TF(NPF(IFUNC)) - XX    
                        COUNTER = 0     

                        DO WHILE(BOOL)
                                MIDDLE = (LAST - FIRST) / 2 + FIRST
                                DX2_FIRST= TF(NPF(IFUNC)+2*FIRST) - XX
                                DX2_LAST= TF(NPF(IFUNC)+2*LAST) - XX
                                DX2_MIDDLE= TF(NPF(IFUNC)+2*MIDDLE) - XX

        
                                PRODUCT_FM = DX2_FIRST*DX2_MIDDLE
                                PRODUCT_ML = DX2_MIDDLE*DX2_LAST

                                IF(PRODUCT_FM<0) THEN
                                        LAST=MIDDLE
                                ELSEIF(PRODUCT_ML<0) THEN
                                        FIRST=MIDDLE
                                ELSE
                                        BOOL=.FALSE.
                                ENDIF
                                IF( (LAST-FIRST)<MIN_GAP) BOOL=.FALSE.
                                !   ---------------------
                                !   check in order to avoid infinite loop
                                COUNTER = COUNTER + 1
                                IF( COUNTER>POINT_NBR ) THEN
                                    !   INFINITE LOOP DETECTED
                                    COUNTER = -1
                                    BOOL=.FALSE.
                                ENDIF
                                !   ---------------------
                        ENDDO
                        !       -----------------------------                
                        !       second shot : (b) classical interpolation with reduced interval

                        !   ------------------------
                        !   INFINITE LOOP DETECTED
                        !   the dichotomy failed to reduce the interval, back to old treatment
                        IF( COUNTER == -1 ) THEN
                                FIRST = 1
                                LAST = POINT_NBR 
                        ENDIF
                        !   ------------------------
                        DX2 = TF(NPF(IFUNC)+2*FIRST-2) - XX            
                        DO J=FIRST,LAST
                                I=NPF(IFUNC)+2*J
                                DX1 = -DX2
                                DX2 = TF(I) - XX
                                IF(DX2>=ZERO.OR.J==LAST)THEN
                                        DIV0 = TF(I) - TF(I-2)
                                        DIV = MAX(ABS(DIV0),EM16)
                                        DIV = SIGN(DIV,DIV0)

                                        DERI = (TF(I+1) - TF(I-1)) / DIV
                                        IF(DX1<=DX2)THEN
                                                FINTER = TF(I-1) + DX1 * DERI
                                        ELSE
                                                FINTER = TF(I+1) - DX2 * DERI
                                        ENDIF
                                        RETURN
                                ENDIF
                        ENDDO
                        !       -----------------------------
                ENDIF   !       end of point_nbr < MIN_GAP
        END IF  !       end of constant function
C
        RETURN
        END
!       ----------------------------------------
