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
!  main.F90  --  SIMPLE CORE (reference skeleton) of the OpenRadioss explicit solver
!  =================================================================================
!
!  PURPOSE
!  -------
!  This file is a WORK-IN-PROGRESS / BACKUP intended to eventually replace resol.F
!  (engine/source/engine/resol.F, ~9800 lines). It is NOT wired into the build:
!  resol_head.F still calls RESOL. main.F90 serves as a documented reference that
!  isolates the "vital core" of the explicit (central-difference) time integration,
!  and will be developed further before any production use.
!
!  The body of RESOL is ~10000 lines because it interleaves dozens of optional
!  features along a single time cycle. Here we keep only the minimal sequence of a
!  pure Lagrangian cycle:
!
!      1. Control / sensors / time management            (MANCTR, sensors)
!      2. External loads                                 (FORCE, gravity, monvol)
!      3. Contact interface sorting                      (INTTRI)
!      4. Element internal forces                        (FORINTS, FORINTC, FORINT, FORINTP)
!      5. Contact forces                                 (INTFOP1/2/8)
!      6. Nodal force assembly                           (ASSPAR, ASSPART) + SPMD exchange
!      7. Rigid body forces                              (RBYFOR)
!      8. Nodal time step                                (DTNODA)
!      9. Acceleration  A = F / M                        (ACCELE) + gravity
!     10. Boundary conditions on A                       (BCS10, FIXVEL, RGWAL0)
!     11. Output (anim / TH / restart)                   (SORTIE_MAIN)
!     12. Velocity integration   V += A*dt               (VELOCITY)
!     13. Displacement integration  D += V*dt , X += D   (DEPLA)
!     14. Nodal boundary conditions                      (BCSN)
!     15. Time advance, stop criteria                    (TT, NCYCLE, MSTOP)
!
!  The argument lists of the called routines are intentionally SCHEMATIC (key
!  arguments + "! ... see resol.F line N"). They must be completed from resol.F
!  during the actual port.
!
!  FEATURES DELIBERATELY REMOVED (compared to resol.F)
!  ---------------------------------------------------
!  Each removal is also flagged, at its location, with a "!! [REMOVED] ..." block.
!
!    * SPH (Smooth Particle Hydrodynamics)         resol.F ~2472, 4277 (SPHPREP/SPHTRI...)
!    * ALE / EULER / ALE-FVM                       resol.F ~3440-3580 (ALEMAIN, ALEFVM_MAIN)
!    * AMS / SMS (Advanced/Selective Mass Scaling) resol.F ~2097-2114, 5921 (SMS_*, DTNODAMS)
!    * Interface TYPE24                            resol.F ~4620, 5470, 9238 (I24*)
!    * Adaptive meshing ADM (remesh)               resol.F ~3327-3379 (ADMDIV, ADMVIT...)
!    * External coupling (coupling_adapter)        resol.F ~8359 (COUPLING_ADVANCE)
!    * RAD2RAD / R2R (multidomain)                 resol.F ~5745, 6233-6263 (IRAD2R, *_IBUF_C)
!    * Sections (/SECT)                            resol.F ~1655, 2631 (SECBUF, SECFCUM, SECTIO)
!    * Flexible bodies FXB (/FXBODY)               resol.F ~2320 (NFXBODY, FXBYFOR, FXBYVIT)
!    * CrackFEM / XFEM / CRK                        resol.F ~1889, 4730-4776 (INIXFEM, CRK_*)
!    * ADYREL (dynamic relaxation /DYREL)          resol.F ~4244, 6069, 8358 (ISTAT==3, ENER_W0)
!    * Stamping + Interface TYPE21                 resol.F ~3151 (NINTSTAMP, INTSTAMP_*)
!    * Interface TYPE18 (ALE/Lagrange coupling)    resol.F ~5393, 7088 (I18MAIN_KINE_1/2)
!    * MOV_FRAM (moving frames)                    resol.F ~7759, 9167 (NUMFRAM, MOV_FRAM)
!    * BLAST (/LOAD/PBLAST)                         resol.F ~2965-2976 (PBLAST_LOAD_COMPUTATION)
!
!  Also not carried over here (outside the "vital core", NOT explicitly requested,
!  to be re-integrated as needed): implicit (IMP_*), thermal (GLOB_THERM), non-local
!  damage (NLOC_DMG), FVMBAG airbags, pinch, seatbelts, IGE (isogeometric), etc.
!
!=======================================================================================================================
!||====================================================================
!||    main                     ../engine/source/engine/main.F90
!||--- called by ------------------------------------------------------
!||    resol_head               ../engine/source/engine/resol_head.F   (to be wired in during the port)
!||--- calls (core) ---------------------------------------------------
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
!   M o d u l e s   (reduced to the core)
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
!   C o m m o n   B l o c k s   (core: counters, sizes, time)
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
!   D u m m y   A r g u m e n t s   (reduced signature)
!-----------------------------------------------
        type(timer_),                intent(inout) :: timers          !< timers
        type(connectivity_),         intent(inout) :: element         !< element connectivity
        type(nodal_arrays_),         intent(inout) :: nodes           !< X, V, A, MS, ICODT ...
        type(mat_elem_),             intent(inout) :: mat_elem        !< materials / properties
        type(elbuf_struct_),         intent(inout) :: elbuf_tab(ngroup)!< element buffers
        type(interfaces_),           intent(inout) :: interfaces      !< contact interfaces
        type(skew_),                 intent(inout) :: skews           !< local frames
        type(sensors_),              intent(inout) :: sensors         !< sensors
        type(output_),               intent(inout) :: output          !< outputs (anim/TH/H3D)
        type(dt_),                   intent(inout) :: dt              !< time step control
        type(loads_),                intent(inout) :: loads           !< loads
        type(h3d_database),          intent(inout) :: h3d_data        !< H3D database
        type(python_),               intent(inout) :: python          !< python functions
        type(rwall_),                intent(inout) :: rwall           !< rigid walls

        integer, intent(in)    :: itask                               !< thread number (SMP)
        integer, intent(in)    :: iparg(nparg,ngroup)                 !< element group parameters
        integer, intent(in)    :: ipm(npropmi,*), igeo(npropgi,*)     !< material / geometry ids
        integer, intent(in)    :: ixs(nixs,*), ixq(nixq,*), ixt(nixt,*)
        integer, intent(in)    :: ixp(nixp,*), ixr(nixr,*), ixtg(nixtg,*)
        integer, intent(in)    :: iskwn(liskn,*)
        integer, intent(in)    :: npby(nnpby,*), lpby(*)              !< rigid bodies
        integer, intent(in)    :: igrv(*), lgrav(*)                   !< gravity
        integer, intent(in)    :: ibcl(*), npc(*)                     !< concentrated loads
        integer, intent(in)    :: ibfv(*)                             !< imposed velocities

        my_real, intent(inout) :: pm(npropm,*), geo(npropg,*)
        my_real, intent(inout) :: rby(nrby,*)                         !< rigid bodies
        my_real, intent(inout) :: agrv(*), fsav(nthvki,*), fzero(3,*)
        my_real, intent(inout) :: forc(*), tf(*)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s   (core)
!-----------------------------------------------
        integer :: mstop                       !< 0 = continue, 1/2 = stop
        integer :: nodft, nodlt                !< nodal loop bounds (SMP)
        integer :: nodftsk, nodltsk            !< per-thread bounds
        integer :: itsk                        !< OpenMP thread id
        my_real :: t_kin                       !< working kinetic energy
!-----------------------------------------------
!   E x t e r n a l   f u n c t i o n s
!-----------------------------------------------
        integer, external :: omp_get_thread_num
!=======================================================================================================================
!   I N I T I A L I Z A T I O N   (outside the loop)
!=======================================================================================================================
!  RESOL_INIT allocates work arrays, reads the restart, initializes interfaces,
!  rigid bodies, outputs, time step... (resol.F line 1635).
!  Keep the call as is; cleaning out the arguments specific to the removed features
!  (SPH, ALE, SMS, FXB, sections...) will be done in a second step.
!
!      call resol_init( ... )        ! see resol.F:1635  -- to be completed
!
        mstop = 0
        nodft = 1
        nodlt = numnod
!
!! [REMOVED] SMS/AMS initialization (SMS_INI_ERR, SMS_INI_PART)     resol.F:2097-2114
!! [REMOVED] XFEM/CRK initialization (INIXFEM)                      resol.F:1889
!! [REMOVED] FXB (flexible bodies) initialization                  resol.F:2320
!! [REMOVED] Stamping/INT21 initialization (INTSTAMP_INIT)         resol.F:3151
!! [REMOVED] ALE initialization (INIT_ALE, SPH sizing)             resol.F:2472
!
!=======================================================================================================================
!   M A I N   T I M E   L O O P
!=======================================================================================================================
!  "While not stopped" loop. In resol.F this is a GOTO 100 ... GOTO 500; we transcribe
!  it as a do-while, more readable, but the semantics are identical.
!
      time_loop: do while (mstop == 0)
!
!---------------------------------------------------------------------
!   1.  C O N T R O L   /   S E N S O R S   /   T I M E
!---------------------------------------------------------------------
!         Read engine control cards, update sensors.
          call manctr(output, sensors, h3d_data)
!
!         Moving skews (/SKEW/MOV): kept (base). Purely kinematic frames stay in
!         the core.  (NEWSKW - resol.F:2681)
!
!         Reset the cycle balances (energies, momentum).
          econt = zero ; edamp = zero ; enint = zero
          xmass = zero ; wplast = zero
!
!         Time step rotation: DT1 <- DT2 ; DT2 reset to a large value, it will be
!         minimized by each contribution (elements, contacts, nodal).
          dt1 = dt2
          dt2 = ep06
!
!! [REMOVED] External coupling sync / advance (COUPLING)            resol.F:8359
!! [REMOVED] RAD2RAD handling (IRAD2R): inter-domain exchanges       resol.F:5745,6233
!
!---------------------------------------------------------------------
!   2.  E X T E R N A L   L O A D S
!---------------------------------------------------------------------
!         Concentrated forces, pressures, imposed forces. FORCE fills NODES%A with
!         the external loads (before the internal forces).
          call force(nodes, ibcl, forc, npc, tf, output, python)   ! ... see resol.F:2901
!
!! [REMOVED] BLAST: PBLAST_LOAD_COMPUTATION (/LOAD/PBLAST)          resol.F:2965-2976
!
!         Monitored volumes / airbags (MONVOL0): kept in base if present.
!         (resol.F:3077)  --  to be completed.
!
!---------------------------------------------------------------------
!   3.  C O N T A C T   I N T E R F A C E   S O R T I N G
!---------------------------------------------------------------------
!         Search for contact candidates (spatial sort) for the kept interfaces
!         (types 7, 11, 25...). Removed types are excluded.
          if (ninter > 0) then
            call inttri(output, timers, interfaces, nodes)          ! ... see resol.F:3211
          end if
!
!! [REMOVED] SPH sort/preparation (SPHPREP, SPHTRI, cells)          resol.F:4277
!! [REMOVED] Adaptive meshing ADM (ADMDIV: subdivision)            resol.F:3327-3379
!! [REMOVED] Interface TYPE18 kinematics (I18MAIN_KINE_1)           resol.F:5393
!
!---------------------------------------------------------------------
!   4.  E L E M E N T   I N T E R N A L   F O R C E S
!---------------------------------------------------------------------
!         Compute internal forces of each element family. Each routine accumulates
!         into the force arrays (FSKY / parith-on, or NODES%A directly).
!
!! [REMOVED] ALE/EULER: ALEMAIN + ALEFVM_MAIN (fluid solver)        resol.F:3467
!
          call forints(timers, elbuf_tab, iparg, pm, geo, ixs)       ! solids   ... resol.F:3547
          call forintc(timers, elbuf_tab, iparg, pm, geo, mat_elem)  ! 4n shells... resol.F:4095
          call forint (timers, python, elbuf_tab, iparg, pm, geo)    ! solids/general resol.F:4180
          call forintp(timers, elbuf_tab, iparg, pm, geo)            ! 3n shells/beams resol.F:4322
!
!---------------------------------------------------------------------
!   5.  C O N T A C T   F O R C E S
!---------------------------------------------------------------------
!         Interface forces for the kept types. INTFOP1/2/8 handle the different
!         interface families (segment/segment, node/segment, edge...).
          if (ninter > 0) then
            call intfop2(output, timers, interfaces, nodes, iparg)   ! ... see resol.F:3809
          end if
!
!! [REMOVED] Interface TYPE24: I24NITSCHFOR3 / I24PXFEM             resol.F:4620,5470
!! [REMOVED] Sections (/SECT): section force balance                resol.F:2631
!
!---------------------------------------------------------------------
!   6.  N O D A L   F O R C E   A S S E M B L Y
!---------------------------------------------------------------------
!         Sum the element contributions onto the nodes. In parith-on, ASSPAR sums
!         the FSKY array deterministically.
          call asspar(nodes, iparg, elbuf_tab)                       ! ... see resol.F:4539
!
!! [REMOVED] XFEM/CRK assembly (ASSPAR_CRK)                         resol.F:4835
!
!         Exchange forces on the domain boundaries (MPI/SPMD).
          if (nspmd > 1) then
!           call spmd_exch_a( nodes%a, ... )                         ! ... see resol.F:4686
          end if
!
!---------------------------------------------------------------------
!   7.  R I G I D   B O D Y   F O R C E S
!---------------------------------------------------------------------
!         Report the forces from the secondary nodes to the main node of each rigid
!         body (before computing the acceleration).
          if (nrbody > 0) then
            call rbyfor(timers, npby, rby, lpby, nodes)              ! ... see resol.F:5565
          end if
!
!! [REMOVED] SMS/AMS: mass-scaling assembly preparation             resol.F (ams_prepare_*)
!! [REMOVED] Flexible bodies FXB: FXBYFOR                           resol.F (FXBYFOR)
!
!---------------------------------------------------------------------
!   8.  N O D A L   T I M E   S T E P
!---------------------------------------------------------------------
!         Compute the stable time step from nodal masses and stiffnesses. DT2 is
!         minimized; it is the step used for integration.
          call dtnoda(nodes, dt2)                                    ! ... see resol.F:5913
!
!! [REMOVED] SMS/AMS time step: DTNODAMS                            resol.F:5921
!! [REMOVED] ADYREL: dynamic relaxation/damping (ISTAT==3)          resol.F:6069
!
!---------------------------------------------------------------------
!   9.  A C C E L E R A T I O N   :   A = F / M
!---------------------------------------------------------------------
!         Final parith-on assembly then division by the nodal mass.
          call asspart(nodes)                                        ! ... see resol.F:6700
!
          call python_begin_openmp(python)
!$OMP     PARALLEL PRIVATE(itsk, nodftsk, nodltsk)
          itsk    = omp_get_thread_num()
          nodftsk = 1 + itsk*numnod/nthread
          nodltsk = (itsk+1)*numnod/nthread
!
          call accele(nodes%a, nodes%ar, nodes%v, nodes%ms, nodes%in, &
                      nodftsk, nodltsk)                              ! ... see resol.F:6754
!
!         Gravity: added to the acceleration (after A=F/M).
          if (ngrav > 0) then
            call gravit(igrv, agrv, lgrav, nodes%a, nodes%ms,        &
                        nodftsk, nodltsk)                            ! ... see resol.F:6948
          end if
!$OMP     END PARALLEL
          call python_end_openmp(python)
!
!---------------------------------------------------------------------
!  10.  B O U N D A R Y   C O N D I T I O N S   O N   A
!---------------------------------------------------------------------
!         Kinematic blockings (BCS10), imposed velocities (FIXVEL), rigid walls
!         (RGWAL0). They correct the acceleration before integration.
          call bcs10(nodft, nodlt, nodes%icodt, nodes%icodr,         &
                     nodes%iskew, nodes%a, nodes%ar, skews%skew)     ! ... see resol.F:7137
!
          if (nfxvel > 0) then
            call fixvel(ibfv, nodes%a, nodes%v, npc, tf)             ! ... see resol.F:7409
          end if
!
!! [REMOVED] Interface TYPE18 kinematics 2 (I18MAIN_KINE_2)         resol.F:7088
!
          if (nrwall > 0) then
            call rgwal0(rwall%nprw, rwall%lprw, nodes%x, nodes%a,    &
                        nodes%v, nodes%ms)                           ! ... see resol.F:7480
          end if
!
!! [REMOVED] MOV_FRAM: moving-frame kinematics (NUMFRAM)            resol.F:7759
!
!---------------------------------------------------------------------
!  11.  O U T P U T   (anim / time-history / restart)
!---------------------------------------------------------------------
!         Write result files at the requested times, handle the stop criteria
!         (returns MSTOP).
          call sortie_main(timers, nodes, output, elbuf_tab, iparg,  &
                           dt, mstop)                                ! ... see resol.F:8297
!
!! [REMOVED] ADYREL: reference energy balance (ENER_W0)             resol.F:8358
!
!---------------------------------------------------------------------
!  12.  V E L O C I T Y   I N T E G R A T I O N   :  V += A*dt
!---------------------------------------------------------------------
          call python_begin_openmp(python)
!$OMP     PARALLEL PRIVATE(itsk, nodftsk, nodltsk)
          itsk    = omp_get_thread_num()
          nodftsk = 1 + itsk*numnod/nthread
          nodltsk = (itsk+1)*numnod/nthread
!
          call velocity(nodes%a, nodes%ar, nodes%v, nodes%vr,        &
                        fzero, nodes%itab)                           ! ... see resol.F:8720
!$OMP     END PARALLEL
          call python_end_openmp(python)
!
!         Deferred inivel (/INIVEL with Tstart or sensor)
          if (loads%ninivelt > 0) then
!           call inivel_start( ... )                                 ! ... see resol.F:8753
          end if
!
!---------------------------------------------------------------------
!  13.  D I S P L A C E M E N T   I N T E G R A T I O N :  D += V*dt
!---------------------------------------------------------------------
!         Pure Lagrangian (IALE+IEULER==0). Update D then X = X0 + D.
          call python_begin_openmp(python)
!$OMP     PARALLEL PRIVATE(itsk, nodftsk, nodltsk)
          itsk    = omp_get_thread_num()
          nodftsk = 1 + itsk*numnod/nthread
          nodltsk = (itsk+1)*numnod/nthread
!
          call depla(nodes%v, nodes%d, nodes%x, nodes%vr, nodes%dr,  &
                     nodes%xdp, nodes%ddp, numnod)                   ! ... see resol.F:8813
!$OMP     END PARALLEL
          call python_end_openmp(python)
!
!! [REMOVED] ALE/EULER branch of the integration (moving grid)      resol.F:8843
!
!---------------------------------------------------------------------
!  14.  N O D A L   B O U N D A R Y   C O N D I T I O N S
!---------------------------------------------------------------------
          call bcsn(nodes%icode, nodes%icodt, nodes%icodr)           ! ... see resol.F:9233
!
!! [REMOVED] Interface TYPE24: edge-to-edge fictive node update     resol.F:9238
!! [REMOVED] MOV_FRAM: final frame update (NUMFRAM)                 resol.F:9167
!
!---------------------------------------------------------------------
!  15.  T I M E   A D V A N C E   A N D   S T O P   C R I T E R I A
!---------------------------------------------------------------------
          ncycle = ncycle + 1
!         Double-precision accumulation then cast (numerical stability).
          tt_double = tt_double + dt2
          tt        = tt_double
!
!         Stop: final time reached, or explicit request, or null time step.
          if (tt >= tstop)      mstop = 2
          if (dt2 <= zero)      mstop = 2
!
      end do time_loop
!
!=======================================================================================================================
!   F I N A L I Z A T I O N   (label 500 in resol.F)
!=======================================================================================================================
!  Write the final restart, free work arrays, balances.
!  (resol.F line 9492 and following.)
!
!      call sortie_error( ... )        ! if MSTOP == 1 (stop on error)
!      call ... deallocations ...
!
        return
      end subroutine main
