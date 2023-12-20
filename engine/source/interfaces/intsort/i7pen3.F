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

Chd|====================================================================
Chd|  I7_PENETRATION                       source/interfaces/intsort/i7pen3.F
Chd|-- called by -----------
Chd|        I7STO                         source/interfaces/intsort/i7sto.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTER7_PENETRATION_MOD
        contains
!< computes the penetration between a chuck of secondary nodes and a main surface/segment
      subroutine inter7_penetration(jlt   ,marge ,x1    ,x2     ,x3   ,
     .                  x4    ,y1    ,y2    ,y3     ,y4   ,
     .                  z1    ,z2    ,z3    ,z4     ,xi   ,
     .                  yi    ,zi    ,pene  ,gapv  ,i3n)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE CONSTANT_MOD
        implicit none
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer jlt, igap
      integer, intent(in) :: i3n ! 0 quad, 1 triangle
      my_real
     .        gapv(jlt), marge 
      my_real
     .     x1, x2, x3, x4, !< x coordinates of the 4 nodes of the surface
     .     y1, y2, y3, y4, !< y coordinates of the 4 nodes of the surface
     .     z1, z2, z3, z4, !< z coordinates of the 4 nodes of the surface
     .     xi(jlt), yi(jlt), zi(jlt), pene(jlt) !< secondary nodes and penetration
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer i, ig
      my_real
     .     x0, y0, z0, gap2(jlt),
     .     nx1, nx2, nx3, nx4,
     .     ny1, ny2, ny3, ny4,
     .     nz1, nz2, nz3, nz4,
     .     lb1(jlt), lb2(jlt), lb3(jlt), lb4(jlt),
     .     lc1(jlt), lc2(jlt), lc3(jlt), lc4(jlt),
     .     al1(jlt), al2(jlt), al3(jlt), al4(jlt),
     .     p1,  p2,  p3,  p4,
     .     x01,  x02,  x03, x04,
     .     y01,  y02,  y03, y04,
     .     z01,  z02,  z03, z04,
     .     xi1(jlt),  xi2(jlt),  xi3(jlt), xi4(jlt),
     .     yi1(jlt),  yi2(jlt),  yi3(jlt), yi4(jlt),
     .     zi1(jlt),  zi2(jlt),  zi3(jlt), zi4(jlt),
     .     hlb1(jlt), hlc1(jlt), hlb2(jlt),hlc2(jlt),
     .     hlb3(jlt), hlc3(jlt), hlb4(jlt),hlc4(jlt)
      my_real
     .     dti, s2,a1,a2,a3,a4,d1,d2,d3,d4,
     .     x12,x23,x34,x41,xi0,sx1,sx2,sx3,sx4,sx0,
     .     y12,y23,y34,y41,yi0,sy1,sy2,sy3,sy4,sy0,
     .     z12,z23,z34,z41,zi0,sz1,sz2,sz3,sz4,sz0,
     .     la, hla, aaa, zoneinf, zoneinf2
c-----------------------------------------------
c gapv(i)=gap+curv_max(cand_e(i))
! the aim is to compute the penetration between a chuck of secondary nodes and a main surface
! the candidate for penetration are couple of secondary nodes and the main surface
! we keep the nodes that are in the zone of influence of the main surface
! we will perform a new collision detection when a node has moved more than the zone of influence (relatively to the main surface)
      do i = 1, jlt
          zoneinf = gapv(i)+marge !< zone of influence: gap of the element + margin 
          gap2(i)= zoneinf*zoneinf
      end do
c
C--------------------------------------------------------
C   Quadrangle 
C--------------------------------------------------------
      if(i3n==0) then
         x0 = fourth*(x1+x2+x3+x4)
         y0 = fourth*(y1+y2+y3+y4)
         z0 = fourth*(z1+z2+z3+z4) 
C--------------------------------------------------------
C  Triangle                      
C--------------------------------------------------------
      else
         x0 = x3
         y0 = y3
         z0 = z3
      endif
c     write(6,*) "I3N=",I3N
c     write(6,*) "X0=",X0,"Y0=",Y0,"Z0=",Z0
c     write(6,*) "X1=",X1,"Y1=",Y1,"Z1=",Z1
c     write(6,*) "X2=",X2,"Y2=",Y2,"Z2=",Z2
c     write(6,*) "X3=",X3,"Y3=",Y3,"Z3=",Z3
c     write(6,*) "X4=",X4,"Y4=",Y4,"Z4=",Z4
C--------------------------------------------------------
C  Triangle
C--------------------------------------------------------
      if(i3n==1) then

       x01 = x1 - x3
       y01 = y1 - y3
       z01 = z1 - z3
c
       x02 = x2 - x3
       y02 = y2 - y3
       z02 = z2 - z3

       x12 = x2 - x1
       y12 = y2 - y1
       z12 = z2 - z1

       do i=1,jlt
         xi0 = x3 - xi(i)
         yi0 = y3 - yi(i)
         zi0 = z3 - zi(i)
c      
         xi1(i) = x1 - xi(i)
         yi1(i) = y1 - yi(i)
         zi1(i) = z1 - zi(i)
c      
         xi2(i) = x2 - xi(i)
         yi2(i) = y2 - yi(i)
         zi2(i) = z2 - zi(i)
c      
         sx1 = yi0*zi1(i) - zi0*yi1(i)
         sy1 = zi0*xi1(i) - xi0*zi1(i)
         sz1 = xi0*yi1(i) - yi0*xi1(i)
c      
         sx2 = yi0*zi2(i) - zi0*yi2(i) 
         sy2 = zi0*xi2(i) - xi0*zi2(i)
         sz2 = xi0*yi2(i) - yi0*xi2(i)
c      
         sx0 = y01*z02 - z01*y02
         sy0 = z01*x02 - x01*z02
         sz0 = x01*y02 - y01*x02
         s2 = 1./max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c      
         lb1(i) = -(sx0*sx2 + sy0*sy2 + sz0*sz2) * s2
         lc1(i) =  (sx0*sx1 + sy0*sy1 + sz0*sz1) * s2
c      
         aaa    = one/max(em30,x01*x01+y01*y01+z01*z01)
         hlc1(i)= lc1(i)*abs(lc1(i))*aaa
         al1(i) = -(xi0*x01+yi0*y01+zi0*z01)*aaa
         al1(i) = max(zero,min(one,al1(i)))
         aaa    = one/max(em30,x02*x02+y02*y02+z02*z02)
         hlb1(i)= lb1(i)*abs(lb1(i))*aaa
         al2(i) = -(xi0*x02+yi0*y02+zi0*z02)*aaa
         al2(i) = max(zero,min(one,al2(i))) 
       enddo
c
      do i=1,jlt
       la = one - lb1(i) - lc1(i)
        aaa = one / max(em20,x12*x12+y12*y12+z12*z12)
        hla= la*abs(la) * aaa
        if(la<zero.and.
     +     hla<=hlb1(i).and.hla<=hlc1(i))then
         lb1(i) = (xi2(i)*x12+yi2(i)*y12+zi2(i)*z12)*aaa
         lb1(i) = max(zero,min(one,lb1(i)))
         lc1(i) = one - lb1(i)
        elseif(lb1(i)<zero.and.
     +        hlb1(i)<=hlc1(i).and.hlb1(i)<=hla)then
         lb1(i) = zero
         lc1(i) = al2(i)
        elseif(lc1(i)<zero.and.
     +        hlc1(i)<=hla.and.hlc1(i)<=hlb1(i))then
         lc1(i) = zero
         lb1(i) = al1(i)
        endif
      enddo
c
      do i=1,jlt
       nx1 = xi(i)-(x3 + lb1(i)*x01 + lc1(i)*x02)
       ny1 = yi(i)-(y3 + lb1(i)*y01 + lc1(i)*y02)
       nz1 = zi(i)-(z3 + lb1(i)*z01 + lc1(i)*z02)
       p1 = nx1*nx1 + ny1*ny1 +nz1*nz1
       pene(i) = max(zero, gap2(i) - p1)
      enddo
c--------------------------------------------------------
      else ! quad

        x01 = x1 - x0
        y01 = y1 - y0
        z01 = z1 - z0
c
        x02 = x2 - x0
        y02 = y2 - y0
        z02 = z2 - z0
c
        x03 = x3 - x0
        y03 = y3 - y0
        z03 = z3 - z0
c
        x04 = x4 - x0
        y04 = y4 - y0
        z04 = z4 - z0
c
        x12 = x2 - x1
        y12 = y2 - y1
        z12 = z2 - z1

        x23 = x3 - x2
        y23 = y3 - y2
        z23 = z3 - z2
        
        x34 = x4 - x3
        y34 = y4 - y3
        z34 = z4 - z3

        x41 = x1 - x4
        y41 = y1 - y4
        z41 = z1 - z4



       do i=1,jlt
c
        xi0 = x0 - xi(i)
        yi0 = y0 - yi(i)
        zi0 = z0 - zi(i)
c
        xi1(i) = x1 - xi(i)
        yi1(i) = y1 - yi(i)
        zi1(i) = z1 - zi(i)
c
        xi2(i) = x2 - xi(i)
        yi2(i) = y2 - yi(i)
        zi2(i) = z2 - zi(i)
c
        xi3(i) = x3 - xi(i)
        yi3(i) = y3 - yi(i)
        zi3(i) = z3 - zi(i)
c
        xi4(i) = x4 - xi(i)
        yi4(i) = y4 - yi(i)
        zi4(i) = z4 - zi(i)
c
        sx1 = yi0*zi1(i) - zi0*yi1(i)
        sy1 = zi0*xi1(i) - xi0*zi1(i)
        sz1 = xi0*yi1(i) - yi0*xi1(i)
c
        sx2 = yi0*zi2(i) - zi0*yi2(i)
        sy2 = zi0*xi2(i) - xi0*zi2(i)
        sz2 = xi0*yi2(i) - yi0*xi2(i)
c
        sx0 = y01*z02 - z01*y02
        sy0 = z01*x02 - x01*z02
        sz0 = x01*y02 - y01*x02
        s2 = one/max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
        lb1(i) = -(sx0*sx2 + sy0*sy2 + sz0*sz2) * s2
        lc1(i) =  (sx0*sx1 + sy0*sy1 + sz0*sz1) * s2
c
        sx3 = yi0*zi3(i) - zi0*yi3(i)
        sy3 = zi0*xi3(i) - xi0*zi3(i)
        sz3 = xi0*yi3(i) - yi0*xi3(i)
c
        sx0 = y02*z03 - z02*y03
        sy0 = z02*x03 - x02*z03
        sz0 = x02*y03 - y02*x03
        s2 = one/max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
        lb2(i) = -(sx0*sx3 + sy0*sy3 + sz0*sz3) * s2
        lc2(i) =  (sx0*sx2 + sy0*sy2 + sz0*sz2) * s2
c
        sx4 = yi0*zi4(i) - zi0*yi4(i)
        sy4 = zi0*xi4(i) - xi0*zi4(i)
        sz4 = xi0*yi4(i) - yi0*xi4(i)
c
        sx0 = y03*z04 - z03*y04
        sy0 = z03*x04 - x03*z04
        sz0 = x03*y04 - y03*x04
        s2 = one/max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
        lb3(i) = -(sx0*sx4 + sy0*sy4 + sz0*sz4) * s2
        lc3(i) =  (sx0*sx3 + sy0*sy3 + sz0*sz3) * s2
c
        sx0 = y04*z01 - z04*y01
        sy0 = z04*x01 - x04*z01
        sz0 = x04*y01 - y04*x01
        s2 = one/max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
        lb4(i) = -(sx0*sx1 + sy0*sy1 + sz0*sz1) * s2
        lc4(i) =  (sx0*sx4 + sy0*sy4 + sz0*sz4) * s2
c
        aaa    = one/max(em30,x01*x01+y01*y01+z01*z01)
        hlc1(i)= lc1(i)*abs(lc1(i))*aaa
        hlb4(i)= lb4(i)*abs(lb4(i))*aaa
        al1(i) = -(xi0*x01+yi0*y01+zi0*z01)*aaa
        al1(i) = max(zero,min(one,al1(i)))
        aaa    = one/max(em30,x02*x02+y02*y02+z02*z02)
        hlc2(i)= lc2(i)*abs(lc2(i))*aaa
        hlb1(i)= lb1(i)*abs(lb1(i))*aaa
        al2(i) = -(xi0*x02+yi0*y02+zi0*z02)*aaa
        al2(i) = max(zero,min(one,al2(i)))
        aaa    = one/max(em30,x03*x03+y03*y03+z03*z03)
        hlc3(i)= lc3(i)*abs(lc3(i))*aaa
        hlb2(i)= lb2(i)*abs(lb2(i))*aaa
        al3(i) = -(xi0*x03+yi0*y03+zi0*z03)*aaa
        al3(i) = max(zero,min(one,al3(i)))
        aaa    = one/max(em30,x04*x04+y04*y04+z04*z04)
        hlc4(i)= lc4(i)*abs(lc4(i))*aaa
        hlb3(i)= lb3(i)*abs(lb3(i))*aaa
        al4(i) = -(xi0*x04+yi0*y04+zi0*z04)*aaa
        al4(i) = max(zero,min(one,al4(i)))
c
       enddo
c
       do i=1,jlt
        la = one - lb1(i) - lc1(i)
        aaa = one / max(em20,x12*x12+y12*y12+z12*z12)
        hla= la*abs(la) * aaa
        if(la<zero.and.
     +     hla<=hlb1(i).and.hla<=hlc1(i))then
         lb1(i) = (xi2(i)*x12+yi2(i)*y12+zi2(i)*z12) * aaa
         lb1(i) = max(zero,min(one,lb1(i)))
         lc1(i) = one - lb1(i)
        elseif(lb1(i)<zero.and.
     +         hlb1(i)<=hlc1(i).and.hlb1(i)<=hla)then
         lb1(i) = zero
         lc1(i) = al2(i)
        elseif(lc1(i)<zero.and.
     +         hlc1(i)<=hla.and.hlc1(i)<=hlb1(i))then
         lc1(i) = zero
         lb1(i) = al1(i)
        endif
       enddo
c
       do i=1,jlt
        la = one - lb2(i) - lc2(i)
        aaa = one / max(em20,x23*x23+y23*y23+z23*z23)
        hla= la*abs(la) * aaa
        if(la<zero.and.
     +     hla<=hlb2(i).and.hla<=hlc2(i))then
         lb2(i) = (xi3(i)*x23+yi3(i)*y23+zi3(i)*z23)*aaa
         lb2(i) = max(zero,min(one,lb2(i)))
         lc2(i) = one - lb2(i)
        elseif(lb2(i)<zero.and.
     +         hlb2(i)<=hlc2(i).and.hlb2(i)<=hla)then
         lb2(i) = zero
         lc2(i) = al3(i)
        elseif(lc2(i)<zero.and.
     +         hlc2(i)<=hla.and.hlc2(i)<=hlb2(i))then
         lc2(i) = zero
         lb2(i) = al2(i)
        endif
       enddo
c
       do i=1,jlt
        la = one - lb3(i) - lc3(i)
        aaa = one / max(em20,x34*x34+y34*y34+z34*z34)
        hla= la*abs(la) * aaa
        if(la<zero.and.
     +     hla<=hlb3(i).and.hla<=hlc3(i))then
         lb3(i) = (xi4(i)*x34+yi4(i)*y34+zi4(i)*z34)*aaa
         lb3(i) = max(zero,min(one,lb3(i)))
         lc3(i) = one - lb3(i)
        elseif(lb3(i)<zero.and.
     +         hlb3(i)<=hlc3(i).and.hlb3(i)<=hla)then
         lb3(i) = zero
         lc3(i) = al4(i)
        elseif(lc3(i)<zero.and.
     +         hlc3(i)<=hla.and.hlc3(i)<=hlb3(i))then
         lc3(i) = zero
         lb3(i) = al3(i)
        endif
       enddo
c
       do i=1,jlt
        la = one - lb4(i) - lc4(i)
        aaa = one / max(em20,x41*x41+y41*y41+z41*z41)
        hla= la*abs(la) * aaa
        if(la<zero.and.
     +     hla<=hlb4(i).and.hla<=hlc4(i))then
         lb4(i) = (xi1(i)*x41+yi1(i)*y41+zi1(i)*z41)*aaa
         lb4(i) = max(zero,min(one,lb4(i)))
         lc4(i) = one - lb4(i)
        elseif(lb4(i)<zero.and.
     +         hlb4(i)<=hlc4(i).and.hlb4(i)<=hla)then
         lb4(i) = zero
         lc4(i) = al1(i)
        elseif(lc4(i)<zero.and.
     +         hlc4(i)<=hla.and.hlc4(i)<=hlb4(i))then
         lc4(i) = zero
         lb4(i) = al4(i)
        endif
       enddo

       do i=1,jlt
c
        nx1 = xi(i)-(x0 + lb1(i)*x01 + lc1(i)*x02)
        ny1 = yi(i)-(y0 + lb1(i)*y01 + lc1(i)*y02)
        nz1 = zi(i)-(z0 + lb1(i)*z01 + lc1(i)*z02)
c       write(6,"(a,3z20)") "xi=",xi(i),yi(i),zi(i)
c       write(6,"(a,3z20)") "x0=",x0,y0,z0
c       write(6,"(a,3z20)") "x01=",x01,y01,z01
c       write(6,"(a,3z20)") "x02=",x02,y02,z02


        p1 = nx1*nx1 + ny1*ny1 +nz1*nz1
        d1 = max(zero, gap2(i) - p1)
c       write(6,"(a,z20,z20)") "p1=",p1,gap2(i)
c       write(6,"(a,z20)") "d1=",d1
c       write(6,"(a,z20)") "lb1=",lb1(i)
c       write(6,"(a,z20)") "lc1=",lc1(i)
c       write(6,"(a,z20,a,z20,a,z20,a,z20)") "x0=",x0,"y0=",y0,"z0=",z0,"xi=",xi(i)
c
        nx2 = xi(i)-(x0 + lb2(i)*x02 + lc2(i)*x03)
        ny2 = yi(i)-(y0 + lb2(i)*y02 + lc2(i)*y03)
        nz2 = zi(i)-(z0 + lb2(i)*z02 + lc2(i)*z03)
        p2 = nx2*nx2 + ny2*ny2 +nz2*nz2
        d2 = max(zero, gap2(i) - p2)
c
        nx3 = xi(i)-(x0 + lb3(i)*x03 + lc3(i)*x04)
        ny3 = yi(i)-(y0 + lb3(i)*y03 + lc3(i)*y04)
        nz3 = zi(i)-(z0 + lb3(i)*z03 + lc3(i)*z04)
        p3 = nx3*nx3 + ny3*ny3 +nz3*nz3
        d3 = max(zero, gap2(i) - p3)
c
        nx4 = xi(i)-(x0 + lb4(i)*x04 + lc4(i)*x01)
        ny4 = yi(i)-(y0 + lb4(i)*y04 + lc4(i)*y01)
        nz4 = zi(i)-(z0 + lb4(i)*z04 + lc4(i)*z01)
        p4 = nx4*nx4 + ny4*ny4 +nz4*nz4
        d4 = max(zero, gap2(i) - p4)
c  pene = gap^2 - dist^2 utilise pour tester si non nul
        pene(i) = max(d1,d2,d3,d4)
c       write(6,*) "pene=",pene(i)
c       write(6,"(a,z20,a,z20,a,z20,a,z20)") "d1=",d1,"d2=",d2,"d3=",d3,"d4=",d4
c
       enddo
      endif
c
      return
      end
      end module