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
      !||    bitget   ../starter/source/interfaces/inter3d1/bitget.F
      !||====================================================================
      INTEGER FUNCTION BITGET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      BITGET = S - (S/2)*2
      RETURN 
      END
      
      !||====================================================================
      !||    bitset   ../starter/source/interfaces/inter3d1/bitget.F
      !||====================================================================
      INTEGER FUNCTION BITSET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      IF(S - (S/2)*2 == 0) I = I + I2P(N)
      BITSET = I
      RETURN 
      END

      !||====================================================================
      !||    bitunset   ../starter/source/interfaces/inter3d1/bitget.F
      !||====================================================================
      INTEGER FUNCTION BITUNSET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      IF(S - (S/2)*2 == 1) I = I - I2P(N)
      BITUNSET = I
      RETURN 
      END

