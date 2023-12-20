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
      MODULE INTER7_GATHER_CAND_MOD
      CONTAINS
Chd|====================================================================
Chd|  I7COR3T                       source/interfaces/int07/i7cor3t.F
Chd|-- called by -----------
Chd|        I10STO                        source/interfaces/intsort/i10sto.F
Chd|        I7STO                         source/interfaces/intsort/i7sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      subroutine inter7_gather_cand(jlt     ,x     ,irect ,nsv   ,cand_e ,
     1                   cand_n  ,igap  ,gap   ,x1    ,x2     ,
     2                   x3      ,x4    ,y1    ,y2    ,y3     ,
     3                   y4      ,z1    ,z2    ,z3    ,z4     ,
     4                   xi      ,yi    ,zi    ,
     5                   nsn     ,gap_s  ,
     6                   gap_m   ,gapv  ,gapmax,gapmin,curv_max,
     7                   ityp    ,gap_s_l,gap_m_l,
     8                   drad    ,dgapload, nsnr,
     .                   s_xrem, xrem)
c-----------------------------------------------
c   i m p l i c i t   t y p e s
c-----------------------------------------------
      implicit none 
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer irect(4,*), nsv(*), cand_e(*), cand_n(*),
     .    jlt,idt, noint, nsn, igap ,ityp
      my_real
     .   x(3,*), gapv(*), gap_s(*), gap_m(*),curv_max(*),
     .   gap, gapmax, gapmin
      my_real , intent(in) :: dgapload ,drad
      my_real
     .   x1, x2, x3, x4,
     .   y1, y2, y3, y4,
     .   z1, z2, z3, z4,
     .   xi(jlt), yi(jlt), zi(jlt), 
     .   gap_s_l(*),gap_m_l(*)
      integer, intent(in) :: s_xrem
      integer, intent(in) :: nsnr
      my_real, intent(in) :: xrem(s_xrem,nsnr)  !< remote (spmd) secondaray data       

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer ix1, ix2, ix3, ix4
      integer i ,j ,il, l, nn, ig,jft,iadd
C-----------------------------------------------
      if(igap==0)then
        !computes 
        do i=1,jlt
          gapv(i)=max(gap+dgapload,drad)
        end do
      elseif(igap == 3)then 
        iadd = 9 
        do i=1,jlt
          j = cand_n(i)
          if(j<=nsn) then 
            gapv(i)=gap_s(j)+gap_m(cand_e(i))
            gapv(i)=min(gap_s_l(j)+gap_m_l(cand_e(i)),gapv(i))
          else
            ig = j-nsn
            gapv(i)=xrem(9,ig)+gap_m(cand_e(i))
            gapv(i)=min(xrem(10,ig)+gap_m_l(cand_e(i)),gapv(i))
          end if
          gapv(i)=min(gapv(i),gapmax)
          gapv(i)=max(gapmin,gapv(i))
          gapv(i)=max(drad,gapv(i)+dgapload)
        end do
      else
        do i=1,jlt
          j = cand_n(i)
          if(j<=nsn) then
            gapv(i)=gap_s(j)+gap_m(cand_e(i)) 
          else
            ig = j-nsn
            gapv(i)=xrem(9,ig)+gap_m(cand_e(i))
          end if
          gapv(i)=min(gapv(i),gapmax)
          gapv(i)=max(gapmin,gapv(i))
          gapv(i)=max(drad,gapv(i)+dgapload)
        end do
      end if
      do i=1,jlt
        j = cand_n(i)
        if(j<=nsn) then
          ig = nsv(j)
          xi(i) = x(1,ig)
          yi(i) = x(2,ig)
          zi(i) = x(3,ig)
        else
          ig = j-nsn
          xi(i) = xrem(1,ig)
          yi(i) = xrem(2,ig)
          zi(i) = xrem(3,ig)
        endif
c
        l  = cand_e(i)
c
      enddo
      ix1=irect(1,l)
      x1=x(1,ix1)
      y1=x(2,ix1)
      z1=x(3,ix1)
c
      ix2=irect(2,l)
      x2=x(1,ix2)
      y2=x(2,ix2)
      z2=x(3,ix2)
c
      ix3=irect(3,l)
      x3=x(1,ix3)
      y3=x(2,ix3)
      z3=x(3,ix3)
c
      ix4=irect(4,l)
      x4=x(1,ix4)
      y4=x(2,ix4)
      z4=x(3,ix4)

      if(ityp == 7)then
        do i=1,jlt
          gapv(i) = gapv(i) + curv_max(cand_e(i))
        end do
      endif
c
      return
      end
      end module

