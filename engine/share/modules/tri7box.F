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
      !||    tri7box          ../starter/share/modules1/tri7box.F
      !||--- called by ------------------------------------------------------
      !||    i11buc_vox1      ../starter/source/interfaces/inter3d1/i11buc1.F
      !||    i11resetvoxel1   ../starter/source/interfaces/inter3d1/i11trivox1.F
      !||    i11trivox1       ../starter/source/interfaces/inter3d1/i11trivox1.F
      !||    i25buc_vox1      ../starter/source/interfaces/inter3d1/i25buc_vox1.F
      !||    i25buce_edg      ../starter/source/interfaces/inter3d1/i25buce_edg.F
      !||    i25cor3_e2s      ../starter/source/interfaces/inter3d1/i25cor3_e2s.F
      !||    i25cor3e         ../starter/source/interfaces/inter3d1/i25cor3e.F
      !||    i25cor3t         ../starter/source/interfaces/inter3d1/i25cor3t.F
      !||    i25pen3_e2s      ../starter/source/interfaces/inter3d1/i25pen3_e2s.F
      !||    i25pen3_edg      ../starter/source/interfaces/inter3d1/i25pen3_edg.F
      !||    i25sto           ../starter/source/interfaces/inter3d1/i25sto.F
      !||    i25sto_e2s       ../starter/source/interfaces/inter3d1/i25sto_e2s.F
      !||    i25sto_edg       ../starter/source/interfaces/inter3d1/i25sto_edg.F
      !||    i25trivox1       ../starter/source/interfaces/inter3d1/i25trivox1.F
      !||    i25trivox_edg    ../starter/source/interfaces/inter3d1/i25trivox_edg.F
      !||    i7buc_vox1       ../starter/source/interfaces/inter3d1/i7buc_vox1.F
      !||    i7trivox1        ../starter/source/interfaces/inter3d1/i7trivox1.F
      !||    spbuc31          ../starter/source/elements/sph/spbuc31.F
      !||    sptrivox         ../starter/source/elements/sph/sptrivox.F
      !||====================================================================
      MODULE TRI7BOX
      USE POINTERDEF
#include "my_real.inc"
      my_real, DIMENSION(:,:), ALLOCATABLE :: XREM
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IREM
C tableau temporaire boite secnd remote pour interface type 7, 10, 11 ou 20
C declare DOUBLE PRECISION pour tranfert entier
C XREM(1) : X(1)
C XREM(2) : X(2)
C XREM(3) : X(3)
C IREM(1) : num local
C XREM(4) : V(1)
C XREM(5) : V(2)
C XREM(6) : V(3)
C XREM(7) : MS
C XREM(8) : STFN
C IREM(2): ITAB
C IREM(3): KINET
C XREM(9): GAP_S (optionnel)
C tableau temporaire boite secnd remote pour interface type 11
C IREM(1) : num local
C XREM(1) : X1(1)
C XREM(2) : X1(2)
C XREM(3) : X1(3)
C XREM(4) : V1(1)
C XREM(5) : V1(2)
C XREM(6) : V1(3)
C XREM(7) : MS1   
C IREM(2):  ITAB1
C XREM(8) : X2(1)
C XREM(9) : X2(2)
C XREM(10) : X2(3)
C XREM(11) : V2(1)
C XREM(12) : V2(2)
C XREM(13) : V2(3)
C XREM(14) : MS2    
C IREM(2):  ITAB2
C XREM(15) : STFS
C IGAP>0
C XREM(16): GAP_S    (optionnel)
C IGAP=3
C XREM(17): GAP_S_L    (optionnel)
C INACTI=5 or 6
C XREM(18): PENIS(1) (optionnel)
C XREM(19): PENIS(2) (optionnel)
C IDTMINS=2
C IREM(4): NODNX_SMS(1) (optionnel)
C IREM(5): N1 (optionnel)
C IREM(6): NODNX_SMS(2) (optionnel)
C IREM(7): N2 (optionnel)
C IREM(4): NODNX_SMS(1) (optionnel)
C IDTMINS_INT/=0
C IREM(4): N1 (optionnel)
C IREM(5): N2 (optionnel)
C tableau temporaire boite secnd remote pour interface type 17
C XREM(1) : EMINXS(1)
C XREM(2) : EMINXS(2)
C XREM(3) : EMINXS(3)
C XREM(4) : EMINXS(4)
C XREM(5) : EMINXS(5)
C XREM(6) : EMINXS(6)
C XREM(7) : num local element
C XREM(8) : XFI17(1,1)  
C XREM(9):  XFI17(2,1)
C XREM(10) :XFI17(3,1)
C XREM(11) :VFI17(1,1) 
C XREM(12) :VFI17(2,1) 
C XREM(13) :VFI17(3,1) 
C XREM(14) :... 
C XREM(98) : XFI17(1,16)
C XREM(99) : XFI17(2,16)    
C XREM(100): XFI17(3,16)
C XREM(101) :VFI17(1,16)
C XREM(102): VFI17(2,16)
C XREM(103): VFI17(3,16)
C XREM(104): FROTSFI(1)
C XREM(105): FROTSFI(2)
C XREM(106): FROTSFI(3)
C XREM(107): FROTSFI(4)
C XREM(108): FROTSFI(5)
C XREM(109): FROTSFI(6)
C XREM(110): FROTSFI(7)
C XREM(111): KSFI(1)
C XREM(112): KSFI(2)

      LOGICAL, DIMENSION(:), ALLOCATABLE :: TO_TRIM, FIRST_TEST
C Pointeur dans XREM vers le debut des valeurs de l interface 24 & 25
C On utlise PANE_OLD
      INTEGER I24XREMP,IGAPXREMP,I24IREMP,I24IREMPNSNE
       
      INTEGER, DIMENSION(:), ALLOCATABLE :: CURRENT_NODFI
      INTEGER, DIMENSION(:), ALLOCATABLE :: CURRENT_FI_SIZE

C buffer envoi/reception forces interfaces
C tableau de pointeurs NSVFI de taille NINTER
C indiquant liste noeuds remote a recevoir du "proc main"
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NSVFI

C Proc main of the secnd node. Assumes that only the main processor sends
C secnds nodes
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: PMAINFI

C tableau de pointeurs NSNFI de taille NINTER
C indiquant pour chaque interface type7 le nb de noeuds secnd dans NSVFI
C par processeur
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: 
     .  NSNFI, NSNFI_SAV, ITAFI, KINFI, MATSFI,IPARTFRICSFI,IF_ADHFI,
     .  IPARTFRIC_FIE,
C tableau i20 specifique (flag nodal)
     .  NBINFLFI,
     .  NODNXFI, NODAMSFI, PROCAMSFI,
C tableau i20 partie edge
     .  NSNFIE,NSVFIE,ITAFIE,NODNXFIE, NODAMSFIE, PROCAMSFIE
C tableaux frontieres interfaces pour MS, STFN, STIFN
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: 
     .  MSFI, STIFI, STNFI, GAPFI,VSCFI, FTHEFI, FTHESKYFI,
     .  AREASFI, TEMPFI, ALPHAKFI, DIAG_SMSFI,GAP_LFI,CONDNFI,
     .  CONDNSKYFI,
C tableau i20 partie edge
     .  MSFIE, STIFIE, STNFIE, GAPFIE, VSCFIE, ALPHAKFIE,
     .  DIAG_SMSFIE,
C tableau I18 Kine
     .  MTFI_PENE,MTFI_PENEMIN
C tableaux frontieres interfaces pour X, V A ou FSKY
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: 
     .  XFI, VFI, AFI, FSKYFI, PENFI, PENFIA,MTFI_V,MTFI_A,
     .  I18KAFI,MTFI_N,DAANCFI,
C tableau i20 partie edge
     .  XFIE, VFIE, AFIE, FSKYFIE, PENFIE, PENFIAE, DAANCFIE,
C NITSCHE METHOD
     .  FORNEQSFI
C tableaux frontieres interfaces 17 pour
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: 
     .  EMINXFI, KSFI, FROTSFI, STNFI17
C tableaux frontieres interfaces 17 pour XFI17, VFI17, AFI17
      TYPE(real_pointer3), DIMENSION(:), ALLOCATABLE :: 
     .  XFI17, VFI17, AFI17
C tableaux frontieres interfaces 20 pour DXANC,DVANC
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: 
     .  DXANCFI, DVANCFI, DXANCFIE, DVANCFIE
C tableaux frontieres interfaces 20 pour DAANC6
      TYPE(r8_pointer3), DIMENSION(:), ALLOCATABLE :: 
     .  DAANC6FI, DAANC6FIE
C tableau de pointeurs ISKYFI de taille NINTER indiquant pour chaque interface
C la liste des noeuds remote associe a une force dans FSKYI
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: ISKYFI,
C tableau i20 partie edge
     .  ISKYFIE
C tableau de pointeurs NSVSI de taille NINTER
C indiquant liste noeuds remote a envoyer au "proc secnd"
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NSVSI,
C tableau i20 partie edge
     .  NSVSIE
C tableau de pointeurs NSNSI de taille NINTER
C indiquant pour chaque interface type7, 10, 11 le nb de noeuds secnd dans NSVSI
C par processeur
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NSNSI,
C tableau i20 partie edge
     .  NSNSIE
C tableau de pointeurs NISUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10 le nbre d elt dans LISUBSFI
C par processeur
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NISUBSFI
C tableau de pointeurs LISUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10, 24, 25 et pour chaque noeud remote de cette interface
C la liste des sous interfaces
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: LISUBSFI
C tableau de pointeurs INFLG_SUBSFI de taille NINTER
C indiquant pour chaque interface type25 et pour chaque noeud remote de cette interface
C l appartenance a S1, S2, ou GRNOD
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: INFLG_SUBSFI ,TAGNCONTFI
C tableau de pointeurs ADDSUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10, 24, 25, et pour chaque noeud remote de cette interface
C son adresse de depart dans LISUBSFI et INFLG_SUBSFI (tableau dimensionne a NSNR+1 (noeud remote + 1)
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: ADDSUBSFI
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: FNCONTI, FTCONTI
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: EFRICFI, EFRICGFI
      INTEGER , DIMENSION(:), ALLOCATABLE :: NLSKYFI, NLSKYFIE
C Temporary array for VOXEL SORTING
      INTEGER , DIMENSION(:), ALLOCATABLE :: NEXT_NOD
      INTEGER , DIMENSION(:), ALLOCATABLE :: CHAIN_ELEM,CHAIN_NEXT
      INTEGER , DIMENSION(:), ALLOCATABLE :: CHAIN_LAST 
      INTEGER , DIMENSION(:), ALLOCATABLE :: PREV_REMOTE_NUMBER 
      INTEGER LVOXEL
      PARAMETER(LVOXEL = 8000000)
      INTEGER INIVOXEL, VOXEL1(LVOXEL)
      DATA INIVOXEL /1/
      INTEGER LRVOXEL,LRVOXELP
C NB do not oversize  31
      PARAMETER(LRVOXEL = 31)
      PARAMETER(LRVOXELP = 8192)
C NB do not oversize PARASIZ
      INTEGER CRVOXEL(0:LRVOXEL,0:LRVOXEL,LRVOXELP)
C
C Flag array to tag if NSNFI was frozen into NSNFI_SAV - Case INACTI + TSTART
      INTEGER , DIMENSION(:), ALLOCATABLE :: NSNFI_FLAG
C Type 24 & 25 Interface
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: ICONT_I_FI
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: IRTLM_FI
C T24 E2E
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: IRTSE_FI
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: IS2SE_FI
      TYPE(int_pointer)  , DIMENSION(:), ALLOCATABLE :: IS2PT_FI
      TYPE(int_pointer)  , DIMENSION(:), ALLOCATABLE :: ISPT2_FI
      TYPE(int_pointer)  , DIMENSION(:), ALLOCATABLE :: ISEGPT_FI
      TYPE(int_pointer)  , DIMENSION(:), ALLOCATABLE :: ISEDGE_FI

      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: TIME_SFI
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: SECND_FRFI
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: PENE_OLDFI
      TYPE(real_pointer2), DIMENSION(:), ALLOCATABLE :: STIF_OLDFI
C Type 25 Interface
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: ISLIDE_FI
      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: REMNOR_FI
      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: KREMNOR_FI
      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: ISKEW_FI
      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: ICODT_FI
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: STIF_MSDT_FI
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: STIFE_MSDT_FI
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: INTAREANFI

C AMS TYPE2 + Contact
      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: T2FAC_SMS_FI
      TYPE(int_pointer2) , DIMENSION(:), ALLOCATABLE :: T2MAIN_SMS_FI

      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: CANDF_SI
cc      my_real
cc     .     BGAPSMXG
      END MODULE TRI7BOX
