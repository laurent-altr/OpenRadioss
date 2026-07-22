!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!=======================================================================================================================
!
!  main.F90  --  COEUR SIMPLE (squelette de reference) du solveur explicite OpenRadioss
!  ======================================================================================
!
!  OBJET
!  -----
!  Ce fichier est une VERSION DE TRAVAIL / BACKUP destinee a remplacer a terme resol.F
!  (engine/source/engine/resol.F, ~9800 lignes). Il n'est PAS branche dans le build :
!  resol_head.F continue d'appeler RESOL. main.F90 sert de reference documentee pour
!  isoler le "coeur vital" de l'integration explicite (schema aux differences centrees)
!  et sera enrichi progressivement avant toute mise en production.
!
!  Le corps de RESOL fait ~10000 lignes car il entrelace des dizaines de fonctionnalites
!  optionnelles au fil d'un unique cycle temporel. Ici on ne conserve que la sequence
!  minimale d'un cycle Lagrangien pur :
!
!      1. Controle / capteurs / gestion du temps         (MANCTR, sensors)
!      2. Chargements externes                            (FORCE, gravite, monvol)
!      3. Tri des interfaces de contact                   (INTTRI)
!      4. Forces internes elementaires                    (FORINTS, FORINTC, FORINT, FORINTP)
!      5. Forces de contact                               (INTFOP1/2/8)
!      6. Assemblage des forces nodales                   (ASSPAR, ASSPART) + echanges SPMD
!      7. Forces des corps rigides                        (RBYFOR)
!      8. Pas de temps nodal                              (DTNODA)
!      9. Acceleration  A = F / M                         (ACCELE) + gravite
!     10. Conditions aux limites sur A                    (BCS10, FIXVEL, RGWAL0)
!     11. Sorties (anim / TH / restart)                   (SORTIE_MAIN)
!     12. Integration vitesse   V += A*dt                 (VELOCITY)
!     13. Integration deplacement  D += V*dt , X += D     (DEPLA)
!     14. Conditions aux limites nodales                  (BCSN)
!     15. Avance du temps, criteres d'arret               (TT, NCYCLE, MSTOP)
!
!  Les listes d'arguments des routines appelees sont volontairement SCHEMATIQUES
!  (arguments cles + "! ... voir resol.F ligne N"). Elles devront etre completees a
!  partir de resol.F lors du portage reel.
!
!  FONCTIONNALITES VOLONTAIREMENT SUPPRIMEES (par rapport a resol.F)
!  ----------------------------------------------------------------
!  Chaque suppression est aussi signalee, a sa position, par un bloc "!! [SUPPRIME] ...".
!
!    * SPH (Smooth Particle Hydrodynamics)        resol.F ~2472, 4277 (SPHPREP/SPHTRI...)
!    * ALE / EULER / ALE-FVM                      resol.F ~3440-3580 (ALEMAIN, ALEFVM_MAIN)
!    * AMS / SMS (Advanced/Selective Mass Scaling) resol.F ~2097-2114, 5921 (SMS_*, DTNODAMS)
!    * Interface TYPE24                           resol.F ~4620, 5470, 9238 (I24*)
!    * Maillage adaptatif ADM (remesh)            resol.F ~3327-3379 (ADMDIV, ADMVIT...)
!    * Couplage (coupling_adapter)                resol.F ~3359? , 8359 (COUPLING_ADVANCE)
!    * RAD2RAD / R2R (multidomaine)               resol.F ~5745, 6233-6263 (IRAD2R, *_IBUF_C)
!    * Sections (/SECT)                           resol.F ~1655, 2631 (SECBUF, SECFCUM, SECTIO)
!    * Corps flexibles FXB (/FXBODY)              resol.F ~2320 (NFXBODY, FXBYFOR, FXBYVIT)
!    * CrackFEM / XFEM / CRK                       resol.F ~1889, 4730-4776 (INIXFEM, CRK_*)
!    * ADYREL (relaxation dynamique /DYREL)       resol.F ~4244, 6069, 8358 (ISTAT==3, ENER_W0)
!    * Stamping + Interface TYPE21                resol.F ~3151 (NINTSTAMP, INTSTAMP_*)
!    * Interface TYPE18 (ALE/Lagrange couplage)   resol.F ~5393, 7088 (I18MAIN_KINE_1/2)
!    * MOV_FRAM (reperes mobiles)                 resol.F ~7759, 9167 (NUMFRAM, MOV_FRAM)
!    * BLAST (/LOAD/PBLAST)                        resol.F ~2965-2976 (PBLAST_LOAD_COMPUTATION)
!
!  Ne sont pas non plus repris ici (hors du "minimum vital" mais NON demandes explicitement,
!  a re-integrer selon besoin) : l'implicite (IMP_*), le thermique (GLOB_THERM), le non-local
!  (NLOC_DMG), les airbags FVMBAG, le pinch, les seatbelts, l'IGE (isogeometrique), etc.
!
!=======================================================================================================================
!||====================================================================
!||    main                     ../engine/source/engine/main.F90
!||--- called by ------------------------------------------------------
!||    resol_head               ../engine/source/engine/resol_head.F   (a brancher lors du portage)
!||--- calls (coeur) --------------------------------------------------
!||    resol_init               ../engine/source/engine/resol_init.F
!||    manctr                   ../engine/source/input/manctr.F
!||    force                    ../engine/source/loads/general/force.F90
!||    gravit                   ../engine/source/loads/general/grav/gravit.F
!||    inttri                   ../engine/source/interfaces/intsort/inttri.F
!||    forints                  ../engine/source/elements/forints.F
!||    forintc                  ../engine/source/elements/forintc.F
!||    forint                   ../engine/source/elements/forint.F
!||    forintp                  ../engine/source/elements/forintp.F
!||    asspar                   ../engine/source/assembly/asspar.F
!||    asspart                  ../engine/source/assembly/asspart.F
!||    rbyfor                   ../engine/source/constraints/general/rbody/rbyfor.F
!||    dtnoda                   ../engine/source/time_step/dtnoda.F
!||    accele                   ../engine/source/assembly/accele.F
!||    bcs10                    ../engine/source/constraints/general/bcs/bcs10.F
!||    fixvel                   ../engine/source/constraints/general/impvel/fixvel.F
!||    rgwal0                   ../engine/source/constraints/general/rwall/rgwal0.F
!||    sortie_main              ../engine/source/output/sortie_main.F
!||    velocity                 ../engine/source/assembly/velocity.F
!||    depla                    ../engine/source/assembly/displacement.F
!||    bcsn                     ../engine/source/constraints/general/bcs/bcsn.F
!||====================================================================
      subroutine main(timers, element, nodes,                          &
                       iparg,  ipm,    igeo,                            &
                       ixs,    ixq,    ixt,    ixp,    ixr,    ixtg,    &
                       pm,     geo,    skews,  iskwn,                   &
                       mat_elem, elbuf_tab, interfaces,                &
                       npby,   rby,    lpby,                           &
                       rwall,                                          &
                       igrv,   agrv,   lgrav,  fsav,   fzero,          &
                       ibcl,   forc,   npc,    tf,                     &
                       ibfv,                                           &
                       sensors, output, dt,    loads,  h3d_data,       &
                       python,  itask )
!-----------------------------------------------
!   M o d u l e s   (reduits au coeur)
!-----------------------------------------------
        use timer_mod
        use connectivity_mod
        use nodal_arrays_mod
        use elbufdef_mod
        use mat_elem_mod
        use interfaces_mod
        use skew_mod
        use sensor_mod
        use output_mod
        use dt_mod
        use loads_mod
        use h3d_mod
        use rwall_mod
        use python_funct_mod
        use groupdef_mod
        use message_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
#include      "implicit_f.inc"
!-----------------------------------------------
!   C o m m o n   B l o c k s   (coeur : compteurs, tailles, temps)
!-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "spmd_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s   (signature reduite)
!-----------------------------------------------
        type(timer_),                intent(inout) :: timers          !< chronometres
        type(connectivity_),         intent(inout) :: element         !< connectivite elements
        type(nodal_arrays_),         intent(inout) :: nodes           !< X, V, A, MS, ICODT ...
        type(mat_elem_),             intent(inout) :: mat_elem        !< materiaux / proprietes
        type(elbuf_struct_),         intent(inout) :: elbuf_tab(ngroup)!< buffers elementaires
        type(interfaces_),           intent(inout) :: interfaces      !< interfaces de contact
        type(skew_),                 intent(inout) :: skews           !< reperes locaux
        type(sensors_),              intent(inout) :: sensors         !< capteurs
        type(output_),               intent(inout) :: output          !< sorties (anim/TH/H3D)
        type(dt_),                   intent(inout) :: dt              !< controle du pas de temps
        type(loads_),                intent(inout) :: loads           !< chargements
        type(h3d_database),          intent(inout) :: h3d_data        !< base H3D
        type(python_),               intent(inout) :: python          !< fonctions python
        type(rwall_),                intent(inout) :: rwall           !< murs rigides

        integer, intent(in)    :: itask                               !< numero de thread (SMP)
        integer, intent(in)    :: iparg(nparg,ngroup)                 !< parametres de groupe
        integer, intent(in)    :: ipm(npropmi,*), igeo(npropgi,*)     !< id materiaux / geometrie
        integer, intent(in)    :: ixs(nixs,*), ixq(nixq,*), ixt(nixt,*)
        integer, intent(in)    :: ixp(nixp,*), ixr(nixr,*), ixtg(nixtg,*)
        integer, intent(in)    :: iskwn(liskn,*)
        integer, intent(in)    :: npby(nnpby,*), lpby(*)              !< corps rigides
        integer, intent(in)    :: igrv(*), lgrav(*)                   !< gravite
        integer, intent(in)    :: ibcl(*), npc(*)                     !< charges concentrees
        integer, intent(in)    :: ibfv(*)                             !< vitesses imposees

        my_real, intent(inout) :: pm(npropm,*), geo(npropg,*)
        my_real, intent(inout) :: rby(nrby,*)                         !< corps rigides
        my_real, intent(inout) :: agrv(*), fsav(nthvki,*), fzero(3,*)
        my_real, intent(inout) :: forc(*), tf(*)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s   (coeur)
!-----------------------------------------------
        integer :: mstop                       !< 0 = continuer, 1/2 = arret
        integer :: nodft, nodlt                !< bornes de boucle nodale (SMP)
        integer :: nodftsk, nodltsk            !< bornes par thread
        integer :: itsk                        !< id thread OpenMP
        my_real :: t_kin                       !< energie cinetique de travail
!-----------------------------------------------
!   E x t e r n a l   f u n c t i o n s
!-----------------------------------------------
        integer, external :: omp_get_thread_num
!=======================================================================================================================
!   I N I T I A L I S A T I O N   (hors boucle)
!=======================================================================================================================
!  RESOL_INIT alloue les tableaux de travail, relit le restart, initialise interfaces,
!  corps rigides, sorties, pas de temps... (resol.F ligne 1635).
!  On garde l'appel tel quel ; le nettoyage des arguments propres aux features supprimees
!  (SPH, ALE, SMS, FXB, sections...) sera fait dans un second temps.
!
!      call resol_init( ... )        ! voir resol.F:1635  -- a completer
!
        mstop = 0
        nodft = 1
        nodlt = numnod
!
!! [SUPPRIME] Initialisations SMS/AMS (SMS_INI_ERR, SMS_INI_PART)   resol.F:2097-2114
!! [SUPPRIME] Initialisation XFEM/CRK (INIXFEM)                     resol.F:1889
!! [SUPPRIME] Initialisation FXB (corps flexibles)                 resol.F:2320
!! [SUPPRIME] Initialisation Stamping/INT21 (INTSTAMP_INIT)        resol.F:3151
!! [SUPPRIME] Initialisation ALE (INIT_ALE, dimensionnement SPH)   resol.F:2472
!
!=======================================================================================================================
!   B O U C L E   T E M P O R E L L E   P R I N C I P A L E
!=======================================================================================================================
!  Boucle "tant que pas d'arret". Dans resol.F c'est un GOTO 100 ... GOTO 500 ; on le
!  transcrit en do-while, plus lisible, mais la semantique est identique.
!
      time_loop: do while (mstop == 0)
!
!---------------------------------------------------------------------
!   1.  C O N T R O L E   /   C A P T E U R S   /   T E M P S
!---------------------------------------------------------------------
!         Lecture des cartes de controle engine, mise a jour des capteurs.
          call manctr(output, sensors, h3d_data)
!
!         Reperes mobiles (/SKEW/MOV) : conserve (base). Les reperes purement
!         cinematiques restent dans le coeur.
!         (NEWSKW - resol.F:2681)
!
!         Remise a zero des bilans du cycle (energies, quantite de mouvement).
          econt = zero ; edamp = zero ; enint = zero
          xmass = zero ; wplast = zero
!
!         Rotation du pas de temps : DT1 <- DT2 ; DT2 reinitialise a une grande valeur,
!         il sera minimise par chaque contribution (elements, contacts, nodal).
          dt1 = dt2
          dt2 = ep06
!
!! [SUPPRIME] Synchronisation / avance du couplage externe (COUPLING)   resol.F:8359
!! [SUPPRIME] Gestion RAD2RAD (IRAD2R) : echanges inter-domaines          resol.F:5745,6233
!
!---------------------------------------------------------------------
!   2.  C H A R G E M E N T S   E X T E R N E S
!---------------------------------------------------------------------
!         Forces concentrees, pressions, forces imposees. FORCE remplit NODES%A
!         avec les efforts externes (avant les efforts internes).
          call force(nodes, ibcl, forc, npc, tf, output, python)   ! ... voir resol.F:2901
!
!! [SUPPRIME] BLAST : PBLAST_LOAD_COMPUTATION (/LOAD/PBLAST)           resol.F:2965-2976
!
!         Volumes monitores / airbags (MONVOL0) : conserve en base si presents.
!         (resol.F:3077)  --  a completer.
!
!---------------------------------------------------------------------
!   3.  T R I   D E S   I N T E R F A C E S   D E   C O N T A C T
!---------------------------------------------------------------------
!         Recherche des candidats au contact (tri spatial) pour les interfaces
!         conservees (types 7, 11, 25...). Les types supprimes sont exclus.
          if (ninter > 0) then
            call inttri(output, timers, interfaces, nodes)          ! ... voir resol.F:3211
          end if
!
!! [SUPPRIME] Tri/preparation SPH (SPHPREP, SPHTRI, cellules)          resol.F:4277
!! [SUPPRIME] Maillage adaptatif ADM (ADMDIV : subdivision)            resol.F:3327-3379
!! [SUPPRIME] Interface TYPE18 cinematique (I18MAIN_KINE_1)            resol.F:5393
!
!---------------------------------------------------------------------
!   4.  F O R C E S   I N T E R N E S   E L E M E N T A I R E S
!---------------------------------------------------------------------
!         Calcul des efforts internes de chaque famille d'elements. Chaque routine
!         accumule dans les tableaux de forces (FSKY / parith-on, ou NODES%A direct).
!
!! [SUPPRIME] ALE/EULER : ALEMAIN + ALEFVM_MAIN (solveur fluide)       resol.F:3467
!
          call forints(timers, elbuf_tab, iparg, pm, geo, ixs)       ! solides  ... resol.F:3547
          call forintc(timers, elbuf_tab, iparg, pm, geo, mat_elem)  ! coques 4n... resol.F:4095
          call forint (timers, python, elbuf_tab, iparg, pm, geo)    ! solides/general resol.F:4180
          call forintp(timers, elbuf_tab, iparg, pm, geo)            ! coques 3n/poutres resol.F:4322
!
!---------------------------------------------------------------------
!   5.  F O R C E S   D E   C O N T A C T
!---------------------------------------------------------------------
!         Efforts d'interface pour les types conserves. INTFOP1/2/8 traitent les
!         differentes familles d'interfaces (segment/segment, node/segment, edge...).
          if (ninter > 0) then
            call intfop2(output, timers, interfaces, nodes, iparg)   ! ... voir resol.F:3809
          end if
!
!! [SUPPRIME] Interface TYPE24 : I24NITSCHFOR3 / I24PXFEM              resol.F:4620,5470
!! [SUPPRIME] Sections (/SECT) : bilan d'efforts de section            resol.F:2631
!
!---------------------------------------------------------------------
!   6.  A S S E M B L A G E   D E S   F O R C E S   N O D A L E S
!---------------------------------------------------------------------
!         Sommation des contributions elementaires vers les noeuds. En parith-on,
!         ASSPAR somme le tableau FSKY de facon deterministe.
          call asspar(nodes, iparg, elbuf_tab)                       ! ... voir resol.F:4539
!
!! [SUPPRIME] Assemblage XFEM/CRK (ASSPAR_CRK)                         resol.F:4835
!
!         Echange des forces sur les frontieres de domaine (MPI/SPMD).
          if (nspmd > 1) then
!           call spmd_exch_a( nodes%a, ... )                         ! ... voir resol.F:4686
          end if
!
!---------------------------------------------------------------------
!   7.  F O R C E S   D E S   C O R P S   R I G I D E S
!---------------------------------------------------------------------
!         Report des efforts des noeuds secondaires vers le noeud maitre de chaque
!         corps rigide (avant calcul de l'acceleration).
          if (nrbody > 0) then
            call rbyfor(timers, npby, rby, lpby, nodes)              ! ... voir resol.F:5565
          end if
!
!! [SUPPRIME] SMS/AMS : preparation de l'assemblage masse-scaling      resol.F (ams_prepare_*)
!! [SUPPRIME] Corps flexibles FXB : FXBYFOR                            resol.F (FXBYFOR)
!
!---------------------------------------------------------------------
!   8.  P A S   D E   T E M P S   N O D A L
!---------------------------------------------------------------------
!         Calcul du pas de temps stable a partir des masses et raideurs nodales.
!         DT2 est minimise ; c'est le pas retenu pour l'integration.
          call dtnoda(nodes, dt2)                                    ! ... voir resol.F:5913
!
!! [SUPPRIME] Pas de temps SMS/AMS : DTNODAMS                          resol.F:5921
!! [SUPPRIME] ADYREL : amortissement/relaxation dynamique (ISTAT==3)   resol.F:6069
!
!---------------------------------------------------------------------
!   9.  A C C E L E R A T I O N   :   A = F / M
!---------------------------------------------------------------------
!         Assemblage parith-on final puis division par la masse nodale.
          call asspart(nodes)                                        ! ... voir resol.F:6700
!
          call python_begin_openmp(python)
!$OMP     PARALLEL PRIVATE(itsk, nodftsk, nodltsk)
          itsk    = omp_get_thread_num()
          nodftsk = 1 + itsk*numnod/nthread
          nodltsk = (itsk+1)*numnod/nthread
!
          call accele(nodes%a, nodes%ar, nodes%v, nodes%ms, nodes%in, &
                      nodftsk, nodltsk)                              ! ... voir resol.F:6754
!
!         Gravite : ajoutee a l'acceleration (apres A=F/M).
          if (ngrav > 0) then
            call gravit(igrv, agrv, lgrav, nodes%a, nodes%ms,        &
                        nodftsk, nodltsk)                            ! ... voir resol.F:6948
          end if
!$OMP     END PARALLEL
          call python_end_openmp(python)
!
!---------------------------------------------------------------------
!  10.  C O N D I T I O N S   A U X   L I M I T E S   S U R   A
!---------------------------------------------------------------------
!         Blocages cinematiques (BCS10), vitesses imposees (FIXVEL), murs rigides
!         (RGWAL0). Elles corrigent l'acceleration avant l'integration.
          call bcs10(nodft, nodlt, nodes%icodt, nodes%icodr,         &
                     nodes%iskew, nodes%a, nodes%ar, skews%skew)     ! ... voir resol.F:7137
!
          if (nfxvel > 0) then
            call fixvel(ibfv, nodes%a, nodes%v, npc, tf)             ! ... voir resol.F:7409
          end if
!
!! [SUPPRIME] Interface TYPE18 cinematique 2 (I18MAIN_KINE_2)          resol.F:7088
!
          if (nrwall > 0) then
            call rgwal0(rwall%nprw, rwall%lprw, nodes%x, nodes%a,    &
                        nodes%v, nodes%ms)                           ! ... voir resol.F:7480
          end if
!
!! [SUPPRIME] MOV_FRAM : reperes mobiles cinematiques (NUMFRAM)        resol.F:7759
!
!---------------------------------------------------------------------
!  11.  S O R T I E S   (anim / time-history / restart)
!---------------------------------------------------------------------
!         Ecriture des fichiers de resultats aux instants demandes, gestion des
!         criteres d'arret (retourne MSTOP).
          call sortie_main(timers, nodes, output, elbuf_tab, iparg,  &
                           dt, mstop)                                ! ... voir resol.F:8297
!
!! [SUPPRIME] ADYREL : bilan d'energie de reference (ENER_W0)          resol.F:8358
!
!---------------------------------------------------------------------
!  12.  I N T E G R A T I O N   D E S   V I T E S S E S   :  V += A*dt
!---------------------------------------------------------------------
          call python_begin_openmp(python)
!$OMP     PARALLEL PRIVATE(itsk, nodftsk, nodltsk)
          itsk    = omp_get_thread_num()
          nodftsk = 1 + itsk*numnod/nthread
          nodltsk = (itsk+1)*numnod/nthread
!
          call velocity(nodes%a, nodes%ar, nodes%v, nodes%vr,        &
                        fzero, nodes%itab)                           ! ... voir resol.F:8720
!$OMP     END PARALLEL
          call python_end_openmp(python)
!
!         inivel differe (/INIVEL avec Tstart ou capteur)
          if (loads%ninivelt > 0) then
!           call inivel_start( ... )                                 ! ... voir resol.F:8753
          end if
!
!---------------------------------------------------------------------
!  13.  I N T E G R A T I O N   D E S   D E P L A C E M E N T S : D += V*dt
!---------------------------------------------------------------------
!         Lagrange pur (IALE+IEULER==0). Met a jour D puis X = X0 + D.
          call python_begin_openmp(python)
!$OMP     PARALLEL PRIVATE(itsk, nodftsk, nodltsk)
          itsk    = omp_get_thread_num()
          nodftsk = 1 + itsk*numnod/nthread
          nodltsk = (itsk+1)*numnod/nthread
!
          call depla(nodes%v, nodes%d, nodes%x, nodes%vr, nodes%dr,  &
                     nodes%xdp, nodes%ddp, numnod)                   ! ... voir resol.F:8813
!$OMP     END PARALLEL
          call python_end_openmp(python)
!
!! [SUPPRIME] Branche ALE/EULER de l'integration (grille mobile)       resol.F:8843
!
!---------------------------------------------------------------------
!  14.  C O N D I T I O N S   A U X   L I M I T E S   N O D A L E S
!---------------------------------------------------------------------
          call bcsn(nodes%icode, nodes%icodt, nodes%icodr)           ! ... voir resol.F:9233
!
!! [SUPPRIME] Interface TYPE24 : mise a jour noeuds fictifs edge-to-edge resol.F:9238
!! [SUPPRIME] MOV_FRAM : mise a jour finale des reperes (NUMFRAM)      resol.F:9167
!
!---------------------------------------------------------------------
!  15.  A V A N C E   D U   T E M P S   E T   C R I T E R E S   D ' A R R E T
!---------------------------------------------------------------------
          ncycle = ncycle + 1
!         Accumulation en double precision puis cast (stabilite numerique).
          tt_double = tt_double + dt2
          tt        = tt_double
!
!         Arret : temps final atteint, ou demande explicite, ou pas de temps nul.
          if (tt >= tstop)      mstop = 2
          if (dt2 <= zero)      mstop = 2
!
      end do time_loop
!
!=======================================================================================================================
!   F I N A L I S A T I O N   (label 500 dans resol.F)
!=======================================================================================================================
!  Ecriture du restart final, liberation des tableaux de travail, bilans.
!  (resol.F ligne 9492 et suivantes.)
!
!      call sortie_error( ... )        ! si MSTOP == 1 (arret sur erreur)
!      call ... deallocations ...
!
        return
      end subroutine main
