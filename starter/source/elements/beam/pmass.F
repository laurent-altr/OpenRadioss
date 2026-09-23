Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    pmass     ../starter/source/elements/beam/pmass.F
!||--- called by ------------------------------------------------------
!||    inivoid   ../starter/source/elements/initia/inivoid.F
!||    pinit3    ../starter/source/elements/beam/pinit3.F
!||====================================================================
      SUBROUTINE PMASS(GEO    ,PM       ,
     .                 STIFN ,STIFR  ,PARTSAV ,V      ,IPART   ,
     .                 MSP   ,INP    ,IGEO    ,STP     ,
     .                 X1,X2,Y1,Y2,Z1,Z2,
     .                 NC1,NC2,IMAT,MXG,AREA,AL,STRP ,
     .                 MCPP ,TEMP ,NINTEMP)
C----------------------------------------------
C     INITIALIZATION OF NODAL MASSES
C----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr12_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(*),IGEO(NPROPGI,*),
     .   NC1(MVSIZ), NC2(MVSIZ), IMAT, MXG(MVSIZ)
      INTEGER, INTENT(IN) ::  NINTEMP
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*),
     .   STIFN(*),STIFR(*),V(3,*),PARTSAV(20,*),MSP(*),INP(*),
     .   STP(*),
     .   X1(MVSIZ), X2(MVSIZ),  
     .   Y1(MVSIZ), Y2(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), AREA(MVSIZ), AL(MVSIZ), STRP(*),
     .   MCPP(*),TEMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IP,I1,I2,J,IPY,IPZ,IPA,IGTYP,NIP
      my_real :: XX,YY,ZZ,XY,YZ,ZX,STI, E, G, AA, BB, ARI,INI,RYI,RZI,STIR, DMP, SL2I, LL,TEMP0
      my_real :: RHO(MVSIZ), EMS(MVSIZ),COEFI(MVSIZ),TIN(MVSIZ), TIXX(MVSIZ), TIYY(MVSIZ), TIZZ(MVSIZ),RHOCP(MVSIZ)
      my_real :: FACDT(MVSIZ), PHII(MVSIZ),KPHI(MVSIZ),CST,PHMAX,PHMIN,FSH(MVSIZ)
      my_real :: SSP,SROT,TIN_TARGET,MARGIN
C=======================================================================
      IPY = 200  
      IPZ = 300  
      IPA = 400
      TEMP0 = PM(79,IMAT)
      IGTYP = 0
      DO I=LFT,LLT
        IGTYP =  nint(GEO(12,MXG(I)))
        IF (IGTYP == 18) THEN
          RHO(I) = PM(89,IMAT)
          AREA(I)= ZERO
          TIYY(I)= ZERO
          TIZZ(I)= ZERO
          NIP    = IGEO(3,MXG(I))
          DO J=1,NIP
            ARI = GEO(IPA+J,MXG(I))
            INI = ARI*ARI*ONE_OVER_12
            RYI = GEO(IPY+J,MXG(I))
            RZI = GEO(IPZ+J,MXG(I))
            AREA(I) = AREA(I) + ARI
            TIYY(I) = TIYY(I) + INI + ARI * RYI*RYI
            TIZZ(I) = TIZZ(I) + INI + ARI * RZI*RZI
          ENDDO
          TIXX(I) = TIYY(I) + TIZZ(I)
          GEO( 1,MXG(I)) = AREA(I) 
          GEO( 4,MXG(I)) = TIXX(I) 
          GEO( 2,MXG(I)) = TIYY(I) 
          GEO(18,MXG(I)) = TIZZ(I) 
        ELSE
          AREA(I)=GEO(1,MXG(I)) 
          TIXX(I)=GEO(4,MXG(I))
          TIYY(I)=GEO(2,MXG(I))
          TIZZ(I)=GEO(18,MXG(I))
          RHO(I) =PM (89,IMAT)
        ENDIF
      ENDDO
C----------------------------------------------
C     for dt
C----------------------------------------------
      DO I=LFT,LLT
        E = PM (20,IMAT)
        G = PM (22,IMAT)
        CST = SIX_OVER_5*E/max(G,em30)
        BB  = MAX(TIYY(I),TIZZ(I),EM30)
        SL2I= AREA(I)*AL(I)**2 /BB
        FACDT(I) = ONE_OVER_12*SL2I
        PHMAX = CST/FACDT(I)        
        PHMIN = MIN(TIYY(I),TIZZ(I))*PHMAX/BB
        KPHI(I) = (FOUR+PHMIN)/(ONE+PHMIN)
        PHII(I) = KPHI(I)/(ONE+FACDT(I))
        PHII(I) = MAX(ONE,PHII(I)) 
        FSH(I) = AL(I)/(FACDT(I)+CST)
        FSH(I) = MAX(ONE,FSH(I))
        COEFI(I) =ONE_OVER_12 
      ENDDO
C
C     TYPE 18 ROTATIONAL INERTIA:
C     TIN has both physical and numerical effects. It is assembled into
C     nodal rotational inertia IN and part inertia PARTSAV; STIR is
C     assembled into STIFR. Its section terms are lumped physical inertia,
C     while ONEP2 and the consistency target with margin add numerical
C     inertia. Other beam types retain their existing PHII treatment.
C     Thus a change to TIN affects rotational dynamics, not only timestep.
C
C     Historical intent: commit 63da9099f0 (October 2023) introduced the
C     TYPE 18 KPHI override and COEFI switch together, completing engine
C     commit c0ec1b679f, titled "improvement on robustness of beam".
C     The extra inertia is therefore part of a deliberate robustness
C     change, consistent with avoiding a restrictive rotational timestep.
C     The commits do not explain the abrupt threshold or establish that
C     its discontinuity was intended or assessed. Numerical stabilization
C     is intentional; the jump is not a physical section transition.
C
C     Let B=MAX(TIYY,TIZZ), F=FACDT=A*L**2/(12*B),
C     Q=RHO*L*B/TWO, and BETA=ONEP2**3=1.728. Define the existing
C     polar-inertia floor INPOL=RHO*L*TIXX/TWO. With COEFI=ONE_OVER_12,
C     the inertia before PHII and the polar-inertia floor is
C
C       TIN0=Q*(ONE+BETA*F).
C
C     The former TYPE 18 treatment overwrote KPHI with MAX(ONE,TWELVE*F),
C     but kept PHII from the generic beam formula. For F > ONE it switched
C     COEFI from ONE_OVER_12 to ONE, multiplying the length-dependent
C     inertia term by TWELVE. PHII was applied only for F < ONE. Thus PHII
C     was not coupled to the TYPE 18 KPHI, and TIN was discontinuous at F=ONE.
C
C     Example: a 20 mm square section represented by four 100 mm2
C     subareas at (RYI,RZI)=(+/-5,+/-5) mm gives AREA=400 mm2 and
C     TIYY=TIZZ=13333.333 mm4. For AL=20 mm, FACDT=ONE. In single
C     precision, the adjacent lengths 19.99999809 and 20.00000191 mm
C     select different COEFI values; the last bits of FACDT depend on
C     arithmetic evaluation. With steel density 7.85E-6 kg/mm3 and
C     E/G=2.6, a standalone Fortran reproduction of the former code shows
C     that TIN jumps from about 2.8553 to 22.7504 kg mm2 per beam end,
C     a factor of 7.97.
C     The mathematical discontinuity also exists in double precision.
C
C     A continuous correction can be derived by requiring consistency
C     between the initial, unscaled timestep estimates. Define
C
C       DAMPFAC=SQRT(ONE+DMP**2)-DMP,
C       SROT=MAX(G*TIXX,KPHI*E*B),
C       SSP=MAX(EM20,PM(27,IMAT)).
C
C     PMASS contributes STIR=SROT/(L*DAMPFAC**2). The unscaled DTNODA
C     rotational estimate is SQRT(TWO*IN/STIFR); for this beam-end
C     contribution alone it is DTN=SQRT(TWO*TIN/STIR). DT1LAWP gives
C     DTE=DAMPFAC*L/SSP for TYPE 18. Requiring DTN >= DTE gives
C
C       TIN_TARGET=HALF*L*SROT/(SSP*SSP).
C
C     DAMPFAC cancels. This is the exact minimum for these estimates,
C     not a unique physical inertia or a general stability proof. The
C     independent DTFAC1(11) and DTFAC1(5) user factors are excluded;
C     comparing scaled estimates would multiply the target by
C     (DTFAC1(5)/DTFAC1(11))**2 for positive factors.
C
C     For the example with SSP**2=E/RHO, the former DTN/DTE was about 0.477
C     just below F=ONE and 1.346 just above it. Nodal timestep control
C     can therefore reduce the timestep or add inertia; element timestep
C     control can raise a stability concern. These estimator comparisons
C     alone do not demonstrate divergence of a complete model. Connected
C     elements, constraints, and the selected timestep controls matter.
C
C     TYPE 18 keeps COEFI=ONE_OVER_12, bypasses the PHII inherited from
C     the generic KPHI, and removes the KPHI > TWELVE switch. The minimum
C     consistency correction would preserve the base and polar floor as
C
C       TIN=MAX(TIN0,INPOL,TIN_TARGET).
C
C     This uses the torsional/bending stiffness and sound speed of the
C     estimates. For valid, continuously varying inputs, KPHI, TIN_TARGET,
C     and final TIN are continuous. For SSP**2=E/RHO with bending
C     controlling, TIN_TARGET=Q*KPHI; the example gives 12.56 kg mm2.
C     Other beam types should retain their current treatment.
C
C     The implemented variant also retains a BETA-based robustness margin
C     on the required inertia:
C
C       MARGIN=(ONE+BETA*F)/(ONE+F),
C       TIN=MAX(TIN0,INPOL,MARGIN*TIN_TARGET).
C
C     For SSP**2=E/RHO with bending controlling, this is equivalent to
C     multiplying TIN0 by PHII=MAX(ONE,KPHI/(ONE+F)) for all F, then
C     applying INPOL. It gives 17.1318 kg mm2 in the example. MARGIN lies
C     between ONE and BETA, but the final DTN/DTE need not be bounded by
C     SQRT(BETA): for this square section at L=2 mm, INPOL dominates and
C     DTN/DTE=SQRT(TWO). MARGIN is an explicit robustness policy, not a
C     requirement of the consistency derivation. Both formulations retain
C     numerical inertia and change the former dynamic response.
C
C     Robustness/timestep tradeoff for the example near F=ONE, assuming
C     SSP**2=E/RHO and bending controlling (unscaled estimates):
C
C       Formulation                  TIN per end (kg mm2)    DTN/DTE
C       Former, just below F=ONE            2.8553             0.477
C       Former, just above F=ONE           22.7504             1.346
C       Minimum target (reference)        12.5600             1.000
C       Implemented target with margin    17.1318             1.168
C
C     The minimum target preserves the initial consistency objective:
C     this beam-end rotational estimate is not smaller than DTE. It does
C     not preserve all historical timestep headroom; reducing inertia
C     above the old threshold reduces the corresponding DTN estimate.
C     The margin variant retains an explicit extra inertia cushion, but
C     still gives less headroom immediately above F=ONE than the old
C     switch (1.168 versus 1.346 in the example).
C
C     For SSP**2=E/RHO with bending controlling, the margin variant and
C     old F > ONE branch have the same leading behavior as F grows:
C
C       TIN_OLD    ~ TWELVE*BETA*Q*F,
C       TIN_MARGIN ~ TWELVE*BETA*Q*F.
C
C     The margin variant is therefore the more conservative candidate
C     relative to the minimum target when retaining the historical
C     robustness intent matters; it does not reproduce the old inertia
C     exactly. Neither variant guarantees unchanged stability or actual
C     global timestep. Matching these estimates is not a stability proof.
C
C     Validation scope: ordinary engine PDLEN3 uses the same KPHI-based
C     rotational stiffness, but its AMS elementary-timestep branch uses
C     FOUR instead of KPHI. The initial target alone does not validate
C     that branch or subsequent large-deformation behavior. Full-solver
C     validation must check threshold sweeps in both precisions,
C     rotational dynamics and energy, nodal versus element timestep
C     control, and AMS. The reproduction above checks the arithmetic,
C     not full-solver stability or regression behavior.
C
C     Guard: apply the target only for positive stiffness and positive
C     material sound speed. Otherwise retain TIN0 and the polar floor;
C     there is no consistency guarantee for a nonpositive sound speed.
C     This includes zero-stiffness void beams. For positive sound speed,
C     retain the DT1LAWP floor, but divide by SSP twice instead of forming
C     SSP*SSP: EM20**2 is subnormal in single precision and can become zero
C     with flush-to-zero enabled. The guard is not material validation.
C
      IF (IGTYP == 18) THEN
        DO I=LFT,LLT
          FSH(I) = ONE
          KPHI(I) = MAX(ONE,TWELVE*FACDT(I))
        ENDDO
      ENDIF
C----------------------------------------------
C     MASSE ELEMENT /2
C----------------------------------------------
      DO I=LFT,LLT
        LL = ONEP2*AL(I)
        EMS(I)=RHO(I)*AL(I)*AREA(I)* HALF
        TIN(I)=ONEP2*EMS(I)*LL**2*COEFI(I) + RHO(I)*(AL(I)*HALF)
     .       * MAX(TIYY(I),TIZZ(I))
        IF (IGTYP == 18) THEN
          E = PM(20,IMAT)
          G = PM(22,IMAT)
          BB = MAX(TIYY(I),TIZZ(I))
          SROT = MAX(G*TIXX(I),KPHI(I)*E*BB)
          SSP = PM(27,IMAT)
          IF (SROT > ZERO .AND. SSP > ZERO) THEN
            SSP = MAX(EM20,SSP)
            TIN_TARGET = (HALF*AL(I)/SSP)*(SROT/SSP)
            MARGIN = (ONE+ONEP2**3*FACDT(I))/(ONE+FACDT(I))
            TIN(I) = MAX(TIN(I),MARGIN*TIN_TARGET)
          ENDIF
        ELSE
          IF (FACDT(I)<ONE) TIN(I)=PHII(I)*TIN(I)
        ENDIF
        TIN(I)= MAX(TIN(I),RHO(I)*AL(I)/TWO*TIXX(I))
      ENDDO
       IF( JTHE > 0 ) THEN
           DO I=LFT,LLT
              RHOCP(I) = PM(69,IMAT)
              MCPP(I)  = RHOCP(I)*AL(I)*AREA(I)* HALF
           ENDDO
       ENDIF
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES + RHOCP 
C----------------------------------------------
      DO I=LFT,LLT
        MSP(I) = EMS(I)
      ENDDO
C----------------------------------------------
C     INERTIES SPHERIQUES
C----------------------------------------------
      DO I=LFT,LLT
        INP(I) = TIN(I)
      ENDDO
C----------------------------------------------
C     INITIALIZATION OF NODAL STIFFNESSES FOR INTERFACES
C----------------------------------------------
      IF(I7STIFS/=0)THEN
        DO I=LFT,LLT
          E = PM (20,IMAT)
          STI = E * AREA(I) / AL(I)
          STP(I) = STI
        ENDDO
      ENDIF
C----------------------------------------------
C     INITIALIZATION OF NODAL STIFFNESSES
C----------------------------------------------
      DO I=LFT,LLT
       E = PM (20,IMAT)
       G = PM (22,IMAT)
C
       DMP =MAX(GEO(16,MXG(I)),GEO(17,MXG(I)))
       DMP =DMP*SQRT(TWO)
       AA  =(SQRT(ONE +DMP*DMP)-DMP)
       AA  = AL(I) * AA * AA
       BB  = MAX(TIYY(I),TIZZ(I))
       STIR = MAX(G*TIXX(I),KPHI(I)*E*BB) / AA
       STI  = FSH(I)*AREA(I) * E / AA
C
       STIFN(NC1(I))=STIFN(NC1(I))+STI
       STIFN(NC2(I))=STIFN(NC2(I))+STI
       STIFR(NC1(I))=STIFR(NC1(I))+STIR
       STIFR(NC2(I))=STIFR(NC2(I))+STIR
       STRP(I)=STIR
      ENDDO
C
      DO I=LFT,LLT
        I1 = NC1(I)
        I2 = NC2(I)
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + TWO*EMS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X1(I)+X2(I))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(Y1(I)+Y2(I))
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(Z1(I)+Z2(I))
        XX = (X1(I)*X1(I)+X2(I)*X2(I))
        XY = (X1(I)*Y1(I)+X2(I)*Y2(I))
        YY = (Y1(I)*Y1(I)+Y2(I)*Y2(I))
        YZ = (Y1(I)*Z1(I)+Y2(I)*Z2(I))
        ZZ = (Z1(I)*Z1(I)+Z2(I)*Z2(I))
        ZX = (Z1(I)*X1(I)+Z2(I)*X2(I))
        PARTSAV(5,IP) =PARTSAV(5,IP) + TWO*TIN(I) + EMS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP) + TWO*TIN(I) + EMS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP) + TWO*TIN(I) + EMS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
        PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
        PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
        PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
      ENDDO
      IF (JTHE > 0) THEN                                          
        IF (NINTEMP > 0 ) THEN                                   
          DO I= LFT,LLT                                          
            IF(TEMP(NC1(I))== ZERO) TEMP(NC1(I)) = TEMP0 
            IF(TEMP(NC2(I))== ZERO) TEMP(NC2(I)) = TEMP0
          ENDDO                                                  
        ELSE                                          
          DO I=LFT,LLT                                           
            TEMP(NC1(I)) = PM(79,IMAT)                           
            TEMP(NC2(I)) = PM(79,IMAT)                         
          ENDDO                                                  
        ENDIF
      ENDIF          
C-----------
      RETURN
      END
