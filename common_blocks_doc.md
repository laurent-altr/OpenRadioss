# OpenRadioss Common Blocks Documentation

## Purpose
This file catalogs COMMON blocks found in `scr*.inc`, `com*.inc`, and `commandline.inc` under the engine/starter include trees. It is meant to support COMMON-to-UDT refactoring by recording file ownership, reference usage counts, member lists, and a practical first-pass interpretation of each field. For large blocks (`COM01`, `COM04`), representative variables were checked against source usage; the rest are bulk-classified from naming and neighboring fields.

**Conventions used here**
- `my_real` is documented as `REAL(WP)`.
- Usage counts use the user-supplied reference counts where available; starter-only extras fall back to observed include-site counts in the current tree.
- `SCR01` and engine `SCR04` use `task_common`; treat them as thread-private COMMON storage, not ordinary process-global blocks.

---

## ⚠️ Thread-Private Common Blocks — Critical Refactoring Hazard

Some COMMON blocks are declared **thread-private** (one independent copy per OpenMP thread).
These require special treatment during refactoring and must **never** be replaced by a plain
shared module variable or a single shared UDT instance.

### How thread-privacy is expressed in the source

The `task_common` macro (defined in `engine/share/spe_inc/task_common.inc`) resolves as follows:

| Build target | `task_common` expands to |
|---|---|
| OpenMP (`_OPENMP` defined) | `COMMON` (plain, requires an explicit `!$OMP THREADPRIVATE` on the next line) |
| Cray Y-MP / T90 | `TASK COMMON` (native thread-local syntax) |
| NEC SX-4 | `LOCAL COMMON` (native thread-local syntax) |
| All others | `COMMON` |

Under a modern OpenMP build the `task_common` keyword alone does **nothing** to make a block
thread-private. Thread-privacy is only guaranteed when an explicit `!$OMP THREADPRIVATE(/block_name/)`
directive immediately follows the `COMMON` declaration.

### Complete inventory of thread-private blocks (all `*.inc` files)

The following blocks carry an explicit `!$OMP THREADPRIVATE` directive (found by grepping all
`*.inc` files):

| Block name | File | Scope | Variables | Notes |
|---|---|---|---|---|
| `SCR04` | `engine/share/includes/scr04_c.inc` | Engine | `NRTM, NRTS, NMN, NSN, NTY, NST, MST, NLINM, NLINS, NLINMA, NLINSA, NMNE, NSNE` | Per-thread contact/link relation counters. Documented in this file. |
| `VECT01` | `engine/share/includes/vect01_c.inc` | Engine | `LFT, LLT, NFT, MTN, IAD, ITY, NPT, JALE, ISMSTR, JEUL, JTUR, JTHE, JLAG, JMULT, JHBE, JIVF, NVAUX, JPOR, JCVT, JSPH, JCLOSE, JPLASOL, IREP, IINT, IHET, IGTYP, ISORTH, ISORTHG, ISRAT, ISROT, ICSEN, IFAILURE, JSMS, ISPH2SOL, IPARTSPH, IGRE, IFORMDT` | **Highly critical.** Defines the element loop bounds (`LFT`/`LLT` = loop-from/loop-to) and element-type flags for each thread's current vectorised work packet. All element force routines read these. |
| `UPLAS` | `engine/share/includes/usrplas_c.inc` | Engine | `U_YELD(MVSIZ), U_ETSE(MVSIZ), U_DEFP(MVSIZ)` | Per-thread yield/tangent-modulus/plastic-strain scratch for user material laws. |
| `UTAG` | `engine/share/includes/usrplas_c.inc` | Engine | `U_TAGPLAS(MVSIZ)` | Per-thread plasticity tag array for user material laws. |
| `UVAR` | `engine/share/includes/usrplas_c.inc` | Engine | `UUVAR(MVSIZ, 5000)` | Per-thread user-variable scratch (up to 5000 user state variables × MVSIZ elements). Very large. |
| `VEC_SPRING_NUM` | `engine/share/includes/vec_spring_num.inc` | Engine | `SPR_NUM(MVSIZ)` | Per-thread spring-element number scratch. |
| `IMPL1_PRIVATE` | `engine/share/includes/impl1_c.inc` | Engine | `NG_IMP` | Per-thread implicit-solver group counter. The parent block `IMPL1` (same file) is **not** thread-private. |
| `UNITS_2` | `starter/share/includes/units_fxbody_c.inc` | Starter | `IFXM_L, IFXS_L` | Per-thread fixed-body master/slave logical unit numbers. The comment in the file explicitly states: *"must be threadprivate in order to avoid bug due to multiple threading in ddsplit"*. |

### Refactoring rules for thread-private blocks

> **⚠️ WARNING**: Violating these rules will introduce data races that are silent in serial
> runs and cause non-deterministic results or crashes under OpenMP.

1. **Do not fold into a shared UDT variable.**  
   A `TYPE(my_type) :: ctx` module variable has one instance. A thread-private COMMON has
   one instance *per thread*. These are not equivalent.

2. **Option A — Keep `!$OMP THREADPRIVATE` on the replacement variable.**  
   If the variables are gathered into a UDT and a module variable is used, declare the variable
   with `!$OMP THREADPRIVATE(ctx)`. This is the minimal-change path but it ties the code to
   OpenMP and keeps the global-state pattern.

3. **Option B — Pass as a dummy argument (preferred for new code).**  
   Allocate a per-thread instance in the parallel region with `omp_get_thread_num()` and pass
   it down the call tree as an `INTENT(INOUT)` argument. This eliminates the global-state
   pattern entirely and makes the threading model explicit.

4. **Option C — Thread-indexed array of UDTs.**  
   `TYPE(task_private_t) :: task_ctx(0:max_threads-1)`, where each thread accesses
   `task_ctx(omp_get_thread_num())`. Avoids dummy-argument threading but still requires
   careful initialisation at the start of each parallel region.

5. **`VECT01` is the most critical block.**  
   It is included in virtually every element force routine. Converting it incorrectly will
   break the entire explicit time-integration loop. It should be addressed last, after all
   other blocks are migrated and the call-tree shape is well understood.

---

## Engine common blocks

### COM01 — Global simulation flags, counters, and feature switches
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com01_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 8224 source include-sites
**Note**: Most widely used common block; effectively the global solver control header.
**Note**: Representative evidence: `NCYCLE` is read in `engine/source/input/manctr.F` and explicit solve logic; `KCONTACT` gates contact processing in starter interface readers and `engine/source/engine/resol.F`; `NITSCHE` allocates/activates Nitsche contact paths in `engine/source/engine/resol.F` and `starter/source/interfaces/int24/...`; `IPART_STACK` drives composite stack allocation in starter and restart I/O in engine; `TH_STRAIN` is toggled by TH reader code and consumed by solid/thick-shell force routines.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| N2D | INTEGER | scalar | 2D model flag. | global solver control | simulation_control_t |
| NCPRI | INTEGER | scalar | Print-control level/counter. | global solver control | simulation_control_t |
| IALE | INTEGER | scalar | ALE formulation enabled flag. | ALE | ale_control_t |
| NGROUP | INTEGER | scalar | Count/flag for `GROUP`. | loads, groups, and BCs | entity_counts_t |
| NCYCLE | INTEGER | scalar | Current explicit cycle counter. | time integration and stability | time_control_t |
| IRUN | INTEGER | scalar | Run/restart index. | global solver control | simulation_control_t |
| IGER | INTEGER | scalar | General solver mode/status flag. | global solver control | simulation_control_t |
| LBUFEL | INTEGER | scalar | Length of the element scratch/work buffer. | global solver control | simulation_control_t |
| IRODDL | INTEGER | scalar | Odd/even or rotational DOF mode flag. | global solver control | simulation_control_t |
| IEULER | INTEGER | scalar | Eulerian formulation enabled flag. | global solver control | simulation_control_t |
| IHSH | INTEGER | scalar | Hourglass/shell option flag. | global solver control | simulation_control_t |
| ITESTV | INTEGER | scalar | Verification/test mode flag. | global solver control | simulation_control_t |
| ITURB | INTEGER | scalar | Turbulence option flag. | global solver control | simulation_control_t |
| ILAG | INTEGER | scalar | Lagrangian formulation flag. | global solver control | simulation_control_t |
| ISECUT | INTEGER | scalar | Section-cut feature enabled flag. | global solver control | simulation_control_t |
| IDAMP | INTEGER | scalar | Global damping option flag. | time integration and stability | time_control_t |
| IRXDP | INTEGER | scalar | Extra precision / XDP mode flag. | global solver control | simulation_control_t |
| NMULT | INTEGER | scalar | Count of multi-domain/multi-run entities. | global solver control | simulation_control_t |
| INTEG8 | INTEGER | scalar | 8-point integration flag. | global solver control | simulation_control_t |
| ISIGI | INTEGER | scalar | Stress initialization flag. | global solver control | simulation_control_t |
| NSPMD | INTEGER | scalar | SPMD domain count. | parallel/runtime | parallel_sync_t |
| LENWA | INTEGER | scalar | Length of working array WA. | global solver control | simulation_control_t |
| NNODS | INTEGER | scalar | Number of nodes per current/local stencil. | global solver control | simulation_control_t |
| NCNOIS | INTEGER | scalar | Noise variable count per sensor. | global solver control | simulation_control_t |
| LCNE0 | INTEGER | scalar | Length of node-to-element connectivity tables. | global solver control | simulation_control_t |
| IPARI0 | INTEGER | scalar | Base offset/index into IPARI tables. | parallel/runtime | parallel_sync_t |
| IMAXIMP | INTEGER | scalar | Maximum implicit-iteration or imposed-count setting. | global solver control | simulation_control_t |
| NNOISER | INTEGER | scalar | Restart copy of noise request count. | noise post-processing | noise_control_t |
| NSPGROUP | INTEGER | scalar | Number of SPMD groups. | loads, groups, and BCs | entity_counts_t |
| IRESMD | INTEGER | scalar | Restart/resume mode flag. | global solver control | simulation_control_t |
| IFRWV | INTEGER | scalar | Wave/restart-output flag. | global solver control | simulation_control_t |
| INTBAG | INTEGER | scalar | Airbag interface/model flag. | airbag/fluid | airbag_control_t |
| ICLOSE | INTEGER | scalar | Closure/contact closing flag. | global solver control | simulation_control_t |
| LICBAG | INTEGER | scalar | Length/index for airbag interface connectivity. | airbag/fluid | airbag_control_t |
| LRCBAG | INTEGER | scalar | Length/index for airbag real work arrays. | airbag/fluid | airbag_control_t |
| LIBAGJET | INTEGER | scalar | Length/index for airbag jet integer data. | airbag/fluid | airbag_control_t |
| LRBAGJET | INTEGER | scalar | Length/index for airbag jet real data. | airbag/fluid | airbag_control_t |
| LIBAGHOL | INTEGER | scalar | Length/index for airbag hole integer data. | airbag/fluid | airbag_control_t |
| LRBAGHOL | INTEGER | scalar | Length/index for airbag hole real data. | airbag/fluid | airbag_control_t |
| ISHFRAM | INTEGER | scalar | Shell-frame output/kinematics flag. | global solver control | simulation_control_t |
| NSHFRONT | INTEGER | scalar | Number of shell fronts/fronts to track. | global solver control | simulation_control_t |
| TRIMAT | INTEGER | scalar | Triangular material/trim option flag. | global solver control | simulation_control_t |
| NSPROC | INTEGER | scalar | Number of solver processes requested/used. | global solver control | simulation_control_t |
| NDSOLV | INTEGER | scalar | Solver decomposition/domain-solver count. | global solver control | simulation_control_t |
| NSBMAX | INTEGER | scalar | Maximum number of state blocks/sub-blocks. | global solver control | simulation_control_t |
| NSVMAXT | INTEGER | scalar | Maximum state-variable count. | global solver control | simulation_control_t |
| NFLOW | INTEGER | scalar | Number of flow entities/options. | airbag/fluid | airbag_control_t |
| ICONDP | INTEGER | scalar | Conduction/thermal coupling flag. | global solver control | simulation_control_t |
| DSNROW | INTEGER | scalar | Row count of dense/sparse network table. | global solver control | simulation_control_t |
| DSNCOL | INTEGER | scalar | Column count of dense/sparse network table. | global solver control | simulation_control_t |
| DSNBLOC | INTEGER | scalar | Block count of dense/sparse network table. | global solver control | simulation_control_t |
| NBUCK | INTEGER | scalar | Buckling feature flag/count. | global solver control | simulation_control_t |
| NFASOLFR | INTEGER | scalar | Count of failure-solution fronts/free faces. | global solver control | simulation_control_t |
| KCONTACT | INTEGER | scalar | Global presence flag for contact interfaces/penalty logic. | contact/interfaces | contact_control_t |
| ICRACK | INTEGER | scalar | Crack/XFEM feature enabled flag. | XFEM/crack | xfem_control_t |
| IMPOSE_DR | INTEGER | scalar | Imposed displacement-rate option flag. | global solver control | simulation_control_t |
| IRIGID_MAT | INTEGER | scalar | Rigid-material option flag. | global solver control | simulation_control_t |
| ISTAMPING | INTEGER | scalar | Stamping workflow enabled flag. | global solver control | simulation_control_t |
| IPLYXFEM | INTEGER | scalar | XFEM ply output/processing flag. | XFEM/crack | xfem_control_t |
| IPLYBCS | INTEGER | scalar | Ply boundary-condition processing flag. | element/material state | material_state_layout_t |
| IREAC | INTEGER | scalar | Reaction-force computation flag. | global solver control | simulation_control_t |
| IGRELEM | INTEGER | scalar | Group-element processing flag. | global solver control | simulation_control_t |
| IREST_MSELT | INTEGER | scalar | Restart handling flag for selected materials/elements. | global solver control | simulation_control_t |
| NTHREAD0 | INTEGER | scalar | Reference thread count. | parallel/runtime | parallel_sync_t |
| IALELAG | INTEGER | scalar | ALE/Lagrange coupling mode flag. | ALE | ale_control_t |
| INVOL | INTEGER | scalar | Volume-monitoring flag. | global solver control | simulation_control_t |
| LCNEPXFEM | INTEGER | scalar | Length/index for XFEM node-element connectivity. | XFEM/crack | xfem_control_t |
| NABFWR | INTEGER | (10) | Per-ABF writer enable/update flags (10 slots). | animation/output | output_control_t |
| ABFILE | INTEGER | (10) | Per-ABF output file handles/state (10 slots). | animation/output | output_control_t |
| INT2PEN | INTEGER | scalar | Interface-2 penalty/contact option flag. | global solver control | simulation_control_t |
| IDAMP_RDOF | INTEGER | scalar | Rotational-DOF damping flag. | time integration and stability | time_control_t |
| LCNECRKXFEM | INTEGER | scalar | Length/index for starter crack/XFEM connectivity. | XFEM/crack | xfem_control_t |
| ICRASH | INTEGER | scalar | Crash-analysis mode flag. | global solver control | simulation_control_t |
| FTEMPVAR21 | INTEGER | scalar | Reserved placeholder field. | global solver control | simulation_control_t |
| INTPLYXFEM | INTEGER | scalar | Interface/XFEM ply coupling flag. | XFEM/crack | xfem_control_t |
| NFILSOL | INTEGER | scalar | Number of solution files / solution-file flag. | global solver control | simulation_control_t |
| IPART_STACK | INTEGER | scalar | Composite stack-part workflow enabled flag. | element/material state | material_state_layout_t |
| ISH3NFRAM | INTEGER | scalar | 3-node shell frame option flag. | global solver control | simulation_control_t |
| IS17_OLD | INTEGER | scalar | Legacy /INTER/TYPE17 compatibility flag. | global solver control | simulation_control_t |
| NSEGQUADFR | INTEGER | scalar | Count of quad facets/segments for fracture/contact. | curves/tables/integration | registry_tables_t |
| NLPRI | INTEGER | scalar | Print/listing level for selected reports. | global solver control | simulation_control_t |
| INTERADHESION | INTEGER | scalar | Global flag for adhesive/thermal interface treatment. | contact/interfaces | contact_control_t |
| NITSCHE | INTEGER | scalar | Global flag enabling Nitsche contact formulation. | contact/interfaces | contact_control_t |
| INISPRI | INTEGER | scalar | Initial spring-state generation flag. | element/material state | material_state_layout_t |
| IMASSI | INTEGER | scalar | Engine added-mass stabilization/reset option. | time integration and stability | time_control_t |
| TH_STRAIN | INTEGER | scalar | Enable time-history strain tensor output. | animation/output | output_control_t |

### COM04 — Entity counts and model cardinalities
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com04_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 10247 source include-sites
**Note**: This is the global size ledger for engine allocations and loops.
**Note**: Representative evidence: `NVENTTOT` is built by starter airbag preprocessing and later sizes TH output arrays; `NUMPLY` sizes composite stack data in starter and animation/restart arrays in engine; `NLOADP_HYD` sizes pressure-load/interface coupling in both starter and engine; `NUMELCXFE`/`NUMELTGXFE` drive XFEM/crack-related storage.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NUMMAT | INTEGER | scalar | Number of materials. | model sizing and allocation | entity_counts_t |
| NUMNOD | INTEGER | scalar | Number of nodes. | model sizing and allocation | entity_counts_t |
| NUMSKW | INTEGER | scalar | Number of skew systems. | model sizing and allocation | entity_counts_t |
| NUMBCS | INTEGER | scalar | Number of boundary conditions. | loads, groups, and BCs | entity_counts_t |
| NANALY | INTEGER | scalar | Number of analysis definitions/options. | model sizing and allocation | entity_counts_t |
| NUMELQ | INTEGER | scalar | Total number of `ELQ` entities. | model sizing and allocation | entity_counts_t |
| NUMELS | INTEGER | scalar | Total number of `ELS` entities. | model sizing and allocation | entity_counts_t |
| NUMELC | INTEGER | scalar | Total number of `ELC` entities. | model sizing and allocation | entity_counts_t |
| NUMELT | INTEGER | scalar | Total number of `ELT` entities. | model sizing and allocation | entity_counts_t |
| NUMGEO | INTEGER | scalar | Total number of `GEO` entities. | model sizing and allocation | entity_counts_t |
| NFUNCT | INTEGER | scalar | Count/flag for `FUNCT`. | curves/tables/integration | registry_tables_t |
| NCONLD | INTEGER | scalar | Count/flag for `CONLD`. | model sizing and allocation | entity_counts_t |
| NINVEL | INTEGER | scalar | Count/flag for `INVEL`. | model sizing and allocation | entity_counts_t |
| NLASER | INTEGER | scalar | Count/flag for `LASER`. | model sizing and allocation | entity_counts_t |
| NINTER | INTEGER | scalar | Number of interfaces. | contact/interfaces | contact_control_t |
| NRWALL | INTEGER | scalar | Count/flag for `RWALL`. | model sizing and allocation | entity_counts_t |
| NRBODY | INTEGER | scalar | Number of rigid bodies. | model sizing and allocation | entity_counts_t |
| NODMAS | INTEGER | scalar | Number of nodal masses. | model sizing and allocation | entity_counts_t |
| NFXVEL | INTEGER | scalar | Number of prescribed velocities. | model sizing and allocation | entity_counts_t |
| NRIVET | INTEGER | scalar | Number of rivets. | model sizing and allocation | entity_counts_t |
| NUMELR | INTEGER | scalar | Total number of `ELR` entities. | model sizing and allocation | entity_counts_t |
| NUMELP | INTEGER | scalar | Total number of `ELP` entities. | model sizing and allocation | entity_counts_t |
| NSECT | INTEGER | scalar | Count/flag for `SECT`. | model sizing and allocation | entity_counts_t |
| NRBAG | INTEGER | scalar | Count/flag for `RBAG`. | airbag/fluid | airbag_control_t |
| NJOINT | INTEGER | scalar | Count/flag for `JOINT`. | model sizing and allocation | entity_counts_t |
| NUMELTG | INTEGER | scalar | Total number of `ELTG` entities. | model sizing and allocation | entity_counts_t |
| NSLAG | INTEGER | scalar | Count/flag for `SLAG`. | model sizing and allocation | entity_counts_t |
| NFACX | INTEGER | scalar | Count/flag for `FACX`. | model sizing and allocation | entity_counts_t |
| NUMPOR | INTEGER | scalar | Total number of `POR` entities. | model sizing and allocation | entity_counts_t |
| NACCELM | INTEGER | scalar | Count/flag for `ACCELM`. | model sizing and allocation | entity_counts_t |
| NPRETEN | INTEGER | scalar | Count/flag for `PRETEN`. | model sizing and allocation | entity_counts_t |
| NVOLU | INTEGER | scalar | Count/flag for `VOLU`. | model sizing and allocation | entity_counts_t |
| NPART | INTEGER | scalar | Count/flag for `PART`. | loads, groups, and BCs | entity_counts_t |
| NSURF | INTEGER | scalar | Count/flag for `SURF`. | loads, groups, and BCs | entity_counts_t |
| NSUBS | INTEGER | scalar | Count/flag for `SUBS`. | model sizing and allocation | entity_counts_t |
| NGRAV | INTEGER | scalar | Count/flag for `GRAV`. | model sizing and allocation | entity_counts_t |
| NRBY2 | INTEGER | scalar | Legacy common-block field `NRBY2`; purpose inferred mainly from naming. | loads, groups, and BCs | entity_counts_t |
| NGRNOD | INTEGER | scalar | Count/flag for `GRNOD`. | model sizing and allocation | entity_counts_t |
| NGRBRIC | INTEGER | scalar | Count/flag for `GRBRIC`. | model sizing and allocation | entity_counts_t |
| NGRQUAD | INTEGER | scalar | Count/flag for `GRQUAD`. | curves/tables/integration | registry_tables_t |
| NGRSHEL | INTEGER | scalar | Count/flag for `GRSHEL`. | model sizing and allocation | entity_counts_t |
| NGRSH3N | INTEGER | scalar | Legacy common-block field `NGRSH3N`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRBEAM | INTEGER | scalar | Count/flag for `GRBEAM`. | element/material state | material_state_layout_t |
| NGRTRUS | INTEGER | scalar | Count/flag for `GRTRUS`. | model sizing and allocation | entity_counts_t |
| NGRSPRI | INTEGER | scalar | Count/flag for `GRSPRI`. | element/material state | material_state_layout_t |
| NLINK | INTEGER | scalar | Count/flag for `LINK`. | model sizing and allocation | entity_counts_t |
| INVSTR | INTEGER | scalar | Integer flag/index for `NVSTR`. | model sizing and allocation | entity_counts_t |
| NSLIN | INTEGER | scalar | Count/flag for `SLIN`. | model sizing and allocation | entity_counts_t |
| NUMELX | INTEGER | scalar | Total number of `ELX` entities. | model sizing and allocation | entity_counts_t |
| NCONX | INTEGER | scalar | Count/flag for `CONX`. | model sizing and allocation | entity_counts_t |
| ISUMNX | INTEGER | scalar | Integer flag/index for `SUMNX`. | model sizing and allocation | entity_counts_t |
| NANIM1D | INTEGER | scalar | Legacy common-block field `NANIM1D`; purpose inferred mainly from naming. | animation/output | output_control_t |
| NR2RLNK | INTEGER | scalar | Legacy common-block field `NR2RLNK`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NUMFRAM | INTEGER | scalar | Total number of `FRAM` entities. | model sizing and allocation | entity_counts_t |
| SCODVER | INTEGER | scalar | Legacy common-block field `SCODVER`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| SMINVER | INTEGER | scalar | Legacy common-block field `SMINVER`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| SSRCVER | INTEGER | scalar | Legacy common-block field `SSRCVER`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NUMELS10 | INTEGER | scalar | Total number of `ELS10` entities. | model sizing and allocation | entity_counts_t |
| NUMELS20 | INTEGER | scalar | Total number of `ELS20` entities. | model sizing and allocation | entity_counts_t |
| IDAMPG | INTEGER | scalar | Integer flag/index for `DAMPG`. | time integration and stability | time_control_t |
| NIBVEL | INTEGER | scalar | Count/flag for `IBVEL`. | model sizing and allocation | entity_counts_t |
| NUMELS16 | INTEGER | scalar | Total number of `ELS16` entities. | model sizing and allocation | entity_counts_t |
| NUMELS8 | INTEGER | scalar | Total number of `ELS8` entities. | model sizing and allocation | entity_counts_t |
| NACTIV | INTEGER | scalar | Count/flag for `ACTIV`. | model sizing and allocation | entity_counts_t |
| NDAMP | INTEGER | scalar | Count/flag for `DAMP`. | time integration and stability | time_control_t |
| NUMELTG6 | INTEGER | scalar | Total number of `ELTG6` entities. | model sizing and allocation | entity_counts_t |
| NUMELS8A | INTEGER | scalar | Total number of `ELS8A` entities. | model sizing and allocation | entity_counts_t |
| NRBYKIN | INTEGER | scalar | Count/flag for `RBYKIN`. | loads, groups, and BCs | entity_counts_t |
| NBCSKIN | INTEGER | scalar | Count/flag for `BCSKIN`. | loads, groups, and BCs | entity_counts_t |
| NSEGFLU | INTEGER | scalar | Count/flag for `SEGFLU`. | model sizing and allocation | entity_counts_t |
| NEBCS | INTEGER | scalar | Count/flag for `EBCS`. | loads, groups, and BCs | entity_counts_t |
| NEXMAD | INTEGER | scalar | Count/flag for `EXMAD`. | model sizing and allocation | entity_counts_t |
| NMADPRT | INTEGER | scalar | Count/flag for `MADPRT`. | model sizing and allocation | entity_counts_t |
| NMADSH4 | INTEGER | scalar | Legacy common-block field `NMADSH4`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NMADSH3 | INTEGER | scalar | Legacy common-block field `NMADSH3`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NMADSOL | INTEGER | scalar | Count/flag for `MADSOL`. | model sizing and allocation | entity_counts_t |
| NMADNOD | INTEGER | scalar | Count/flag for `MADNOD`. | model sizing and allocation | entity_counts_t |
| NFXBODY | INTEGER | scalar | Count/flag for `FXBODY`. | model sizing and allocation | entity_counts_t |
| NEIG | INTEGER | scalar | Count/flag for `EIG`. | model sizing and allocation | entity_counts_t |
| NINTSUB | INTEGER | scalar | Count/flag for `INTSUB`. | model sizing and allocation | entity_counts_t |
| NVENTTOT | INTEGER | scalar | Total number of monitored airbag vents. | airbag/fluid | airbag_control_t |
| NUMELC0 | INTEGER | scalar | Total number of `ELC0` entities. | model sizing and allocation | entity_counts_t |
| NUMELTG0 | INTEGER | scalar | Total number of `ELTG0` entities. | model sizing and allocation | entity_counts_t |
| NUMNOD0 | INTEGER | scalar | Total number of `NOD0` entities. | model sizing and allocation | entity_counts_t |
| NLEVSET | INTEGER | scalar | Count/flag for `LEVSET`. | model sizing and allocation | entity_counts_t |
| NDETO | INTEGER | scalar | Count/flag for `DETO`. | model sizing and allocation | entity_counts_t |
| NRBYM | INTEGER | scalar | Count/flag for `RBYM`. | loads, groups, and BCs | entity_counts_t |
| NGSLNRBYM | INTEGER | scalar | Count/flag for `GSLNRBYM`. | loads, groups, and BCs | entity_counts_t |
| NFRBYM | INTEGER | scalar | Count/flag for `FRBYM`. | loads, groups, and BCs | entity_counts_t |
| NIRBYM | INTEGER | scalar | Count/flag for `IRBYM`. | loads, groups, and BCs | entity_counts_t |
| NRBE3 | INTEGER | scalar | Legacy common-block field `NRBE3`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRPART | INTEGER | scalar | Number of part groups. | loads, groups, and BCs | entity_counts_t |
| NGPE | INTEGER | scalar | Count/flag for `GPE`. | model sizing and allocation | entity_counts_t |
| NTHPART | INTEGER | scalar | Count/flag for `THPART`. | loads, groups, and BCs | entity_counts_t |
| NTABLE | INTEGER | scalar | Count/flag for `TABLE`. | curves/tables/integration | registry_tables_t |
| NRBE2 | INTEGER | scalar | Legacy common-block field `NRBE2`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NHRBE2 | INTEGER | scalar | Legacy common-block field `NHRBE2`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NUMELCXFE | INTEGER | scalar | Count of XFEM cohesive/crack-coupled elements. | XFEM/crack | xfem_control_t |
| NUMELTGXFE | INTEGER | scalar | Count of XFEM thick-shell crack-coupled elements. | XFEM/crack | xfem_control_t |
| NLOAD | INTEGER | scalar | Count/flag for `LOAD`. | loads, groups, and BCs | entity_counts_t |
| NLOADC | INTEGER | scalar | Count/flag for `LOADC`. | loads, groups, and BCs | entity_counts_t |
| NLOADP | INTEGER | scalar | Count/flag for `LOADP`. | loads, groups, and BCs | entity_counts_t |
| SIZFIELD | INTEGER | scalar | Legacy common-block field `SIZFIELD`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| SIZLOADP | INTEGER | scalar | Legacy common-block field `SIZLOADP`; purpose inferred mainly from naming. | loads, groups, and BCs | entity_counts_t |
| NBGAUGE | INTEGER | scalar | Count/flag for `BGAUGE`. | model sizing and allocation | entity_counts_t |
| NUMNODXFE | INTEGER | scalar | Number of XFEM enriched nodes. | XFEM/crack | xfem_control_t |
| NCLUSTER | INTEGER | scalar | Count/flag for `CLUSTER`. | model sizing and allocation | entity_counts_t |
| NUMELIG3D | INTEGER | scalar | Total number of `ELIG3D` entities. | model sizing and allocation | entity_counts_t |
| ISFEM | INTEGER | scalar | Integer flag/index for `SFEM`. | model sizing and allocation | entity_counts_t |
| NLOADP_F | INTEGER | scalar | Legacy common-block field `NLOADP_F`; purpose inferred mainly from naming. | loads, groups, and BCs | entity_counts_t |
| NUMNOR | INTEGER | scalar | Total number of `NOR` entities. | model sizing and allocation | entity_counts_t |
| NINTER25 | INTEGER | scalar | Legacy common-block field `NINTER25`; purpose inferred mainly from naming. | contact/interfaces | contact_control_t |
| NINTER25E | INTEGER | scalar | Legacy common-block field `NINTER25E`; purpose inferred mainly from naming. | contact/interfaces | contact_control_t |
| NUMPLY | INTEGER | scalar | Number of plies. | element/material state | material_state_layout_t |
| NUMSTACK | INTEGER | scalar | Number of composite stacks. | element/material state | material_state_layout_t |
| NS10E | INTEGER | scalar | Legacy common-block field `NS10E`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NINTERFRIC | INTEGER | scalar | Count/flag for `INTERFRIC`. | contact/interfaces | contact_control_t |
| NUMSKIN | INTEGER | scalar | Total number of `SKIN` entities. | model sizing and allocation | entity_counts_t |
| NBCSCYC | INTEGER | scalar | Count/flag for `BCSCYC`. | loads, groups, and BCs | entity_counts_t |
| NRBFAIL | INTEGER | scalar | Count/flag for `RBFAIL`. | model sizing and allocation | entity_counts_t |
| NLOADP_HYD | INTEGER | scalar | Number of hydrodynamic pressure loads. | loads, groups, and BCs | entity_counts_t |
| NINTLOADP | INTEGER | scalar | Number of interface-coupled pressure-load relations. | loads, groups, and BCs | entity_counts_t |
| NINTLOADP21 | INTEGER | scalar | Number of type-21 pressure-load couplings. | loads, groups, and BCs | entity_counts_t |
| NLOADP_HYD_INTER | INTEGER | scalar | Size/count of pressure-load/interface coupling records. | contact/interfaces | contact_control_t |
| NTSHEG | INTEGER | scalar | Count/flag for `TSHEG`. | model sizing and allocation | entity_counts_t |
| NRBODY0 | INTEGER | scalar | Original rigid-body count. | model sizing and allocation | entity_counts_t |

### COM06 — Time-step factors, damping, and output intervals
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com06_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 860 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| DTFAC | REAL(WP) | scalar | Global time-step scale factor. | time integration and stability | time_control_t |
| DTMIN | REAL(WP) | scalar | Minimum allowed time step. | time integration and stability | time_control_t |
| VOLMIN | REAL(WP) | scalar | Minimum element/control volume threshold. | time integration and damping | time_control_t |
| REINT | REAL(WP) | scalar | Internal energy ratio/scale. | time integration and damping | time_control_t |
| UREINT | REAL(WP) | scalar | User/internal energy reference. | time integration and damping | time_control_t |
| ECONTV | REAL(WP) | scalar | Contact energy value. | time integration and damping | time_control_t |
| EHOUR | REAL(WP) | scalar | Hourglass energy value. | time integration and damping | time_control_t |
| DTOUTP | REAL(WP) | scalar | Output period for state output. | animation/output | output_control_t |
| TOUTP | REAL(WP) | scalar | Next state output time. | animation/output | output_control_t |
| T1S | REAL(WP) | scalar | Time scale for shell/solid subcycling. | time integration and damping | time_control_t |
| DT2S | REAL(WP) | scalar | Secondary time-step scale. | time integration and stability | time_control_t |
| EPOR | REAL(WP) | scalar | Porosity energy or work accumulator. | time integration and damping | time_control_t |
| R2RFX1 | REAL(WP) | scalar | Rad2Rad coupling factor 1. | time integration and damping | time_control_t |
| R2RFX2 | REAL(WP) | scalar | Rad2Rad coupling factor 2. | time integration and damping | time_control_t |
| DAMPA | REAL(WP) | scalar | Rayleigh damping coefficient A. | time integration and stability | time_control_t |
| DAMPB | REAL(WP) | scalar | Rayleigh damping coefficient B. | time integration and stability | time_control_t |
| DAMPW | REAL(WP) | scalar | Damping work/weighting factor. | time integration and stability | time_control_t |
| DWMAD | REAL(WP) | scalar | MAD damping/work threshold. | curves/tables/integration | registry_tables_t |
| T1SH | REAL(WP) | scalar | Shell-related time scale. | time integration and damping | time_control_t |
| SHFTBUCK | REAL(WP) | scalar | Buckling shift factor. | time integration and damping | time_control_t |
| DTSTAT | REAL(WP) | scalar | Status print period. | animation/output | output_control_t |
| TSTAT | REAL(WP) | scalar | Next status print time. | animation/output | output_control_t |
| DTABF | REAL(WP) | (10) | ABF output periods (10 slots). | animation/output | output_control_t |
| DTABFWR | REAL(WP) | (10) | ABF write periods (10 slots). | animation/output | output_control_t |
| DTFACX | REAL(WP) | scalar | Extra/global time-step factor. | time integration and stability | time_control_t |
| ECONTD | REAL(WP) | scalar | Contact energy increment. | time integration and damping | time_control_t |
| ECONT_CUMU | REAL(WP) | scalar | Cumulative contact energy. | time integration and damping | time_control_t |

### COM08 — Current time state and scheduling values
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com08_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 3964 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| TT | REAL(WP) | scalar | Current physical time. | time integration and stability | time_control_t |
| DT1 | REAL(WP) | scalar | Current stable time step. | time integration and stability | time_control_t |
| DT2 | REAL(WP) | scalar | Next/proposed time step. | time integration and stability | time_control_t |
| DT12 | REAL(WP) | scalar | Half-step or predictor-corrector time increment. | time integration and stability | time_control_t |
| DT2OLD | REAL(WP) | scalar | Previous secondary time step. | time integration and stability | time_control_t |
| TSTOP | REAL(WP) | scalar | Final stop time. | time integration and stability | time_control_t |
| TABFIS | REAL(WP) | (10) | ABF/FIS scheduling table (10 slots). | animation/output | output_control_t |
| TABFWR | REAL(WP) | (10) | ABF write scheduling table (10 slots). | animation/output | output_control_t |
| XMIN_GLOB | REAL(WP) | scalar | Global minimum X coordinate. | time integration and scheduling | time_control_t |
| XMAX_GLOB | REAL(WP) | scalar | Global maximum X coordinate. | time integration and scheduling | time_control_t |
| YMIN_GLOB | REAL(WP) | scalar | Global minimum Y coordinate. | time integration and scheduling | time_control_t |
| YMAX_GLOB | REAL(WP) | scalar | Global maximum Y coordinate. | time integration and scheduling | time_control_t |
| ZMIN_GLOB | REAL(WP) | scalar | Global minimum Z coordinate. | time integration and scheduling | time_control_t |
| ZMAX_GLOB | REAL(WP) | scalar | Global maximum Z coordinate. | time integration and scheduling | time_control_t |

### COM08DP — Double-precision absolute time mirror
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com08_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 3964 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| TT_DP | INTEGER | scalar | Double-precision copy of current physical time. | time integration and stability | time_control_t |

### COM09 — Output-cycle flags and last-write markers
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com09_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 152 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IPAT | INTEGER | scalar | Pattern / output pattern selector. | animation/output scheduling | output_control_t |
| IANIM | INTEGER | scalar | Animation output enabled flag. | animation/output | output_control_t |
| IOUTP | INTEGER | scalar | State-output enabled flag. | animation/output | output_control_t |
| NHIN2 | INTEGER | scalar | HIN/H3D-style history/input option flag. | animation/output scheduling | output_control_t |
| IDELI7 | INTEGER | scalar | Deletion flag for interface type 7. | animation/output scheduling | output_control_t |
| IBAGSURF | INTEGER | scalar | Airbag surface output flag. | airbag/fluid | airbag_control_t |
| ISTATF | INTEGER | scalar | Status file/output flag. | animation/output | output_control_t |
| LASTANIMCYCLE | INTEGER | scalar | Cycle of last animation output. | animation/output | output_control_t |
| ILASTANIM | INTEGER | scalar | Flag/index of last animation record. | animation/output | output_control_t |
| ILASTH3D | INTEGER | scalar | Flag/index of last H3D output. | animation/output | output_control_t |
| LASTH3DCYCLE | INTEGER | scalar | Cycle of last H3D output. | animation/output | output_control_t |
| LASTDYNCYCLE | INTEGER | scalar | Cycle of last DYNAIN output. | time integration and stability | time_control_t |
| ILASTDYNAIN | INTEGER | scalar | Flag/index of last DYNAIN output. | animation/output | output_control_t |
| LASTSTATCYCLE | INTEGER | scalar | Cycle of last status output. | animation/output | output_control_t |

### COM10 — Selected entity counts
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com10_c.inc`
**Present in**: both (same block names and member order)
**Usage count**: 116 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NSNOD | INTEGER | scalar | Selected node count. | selection and filtering | selected_counts_t |
| NSELQ | INTEGER | scalar | Selected ELQ entity count. | selection and filtering | selected_counts_t |
| NSELS | INTEGER | scalar | Selected ELS entity count. | selection and filtering | selected_counts_t |
| NSELC | INTEGER | scalar | Selected ELC entity count. | selection and filtering | selected_counts_t |
| NSELT | INTEGER | scalar | Selected ELT entity count. | selection and filtering | selected_counts_t |
| NSELP | INTEGER | scalar | Selected ELP entity count. | selection and filtering | selected_counts_t |
| NSELR | INTEGER | scalar | Selected ELR entity count. | selection and filtering | selected_counts_t |
| NSRBY | INTEGER | scalar | Selected rigid-body count. | loads, groups, and BCs | entity_counts_t |
| NSMAT | INTEGER | scalar | Selected material count. | selection and filtering | selected_counts_t |
| NSELTG | INTEGER | scalar | Selected thick-shell/group entity count. | selection and filtering | selected_counts_t |
| NTHGRP | INTEGER | scalar | Time-history group count. | selection and filtering | selected_counts_t |
| NTHGRP0 | INTEGER | scalar | Baseline/original time-history group count. | selection and filtering | selected_counts_t |
| NSELU | INTEGER | scalar | Selected user-defined entity count. | selection and filtering | selected_counts_t |
| NTHGRP01 | INTEGER | (10) | Per-domain/group baseline TH counts (10 slots). | selection and filtering | selected_counts_t |
| NTHGRP1 | INTEGER | (10) | Per-domain/group TH counts (10 slots). | selection and filtering | selected_counts_t |
| NSFXBY | INTEGER | scalar | Selected fixed-body count. | selection and filtering | selected_counts_t |
| NSMOD | INTEGER | scalar | Selected model count. | selection and filtering | selected_counts_t |

### COM20 — Quadrature and integration tables
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com20_c.inc`
**Present in**: engine only
**Usage count**: 71 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| Z0 | REAL(WP) | (11,11) | Quadrature abscissa table. | numerical integration | quadrature_tables_t |
| WF | REAL(WP) | (11,11) | Full integration weight table. | numerical integration | quadrature_tables_t |
| WM | REAL(WP) | (11,11) | Mass integration weight table. | numerical integration | quadrature_tables_t |
| ZTH | REAL(WP) | (12,11) | Through-thickness integration table. | numerical integration | quadrature_tables_t |

### XFEMI — XFEM global counts and crack topology sizes
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/com_xfem1.inc`
**Present in**: both (starter content differs)
**Usage count**: 667 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NUMELCRK | INTEGER | scalar | Number of crack/XFEM elements. | XFEM/crack | xfem_control_t |
| ICRACK3D | INTEGER | scalar | 3D crack processing flag. | XFEM/crack | xfem_control_t |
| NXEL | INTEGER | scalar | Number of enriched XFEM elements. | XFEM/crack | xfem_control_t |
| NCRKPART | INTEGER | scalar | Number of cracked parts. | XFEM/crack | xfem_control_t |
| NCRKXFE | INTEGER | scalar | Number of XFEM crack entities. | XFEM/crack | xfem_control_t |
| ECRKXFE | INTEGER | scalar | Count/size of XFEM crack-edge data. | XFEM/crack | xfem_control_t |
| ECRKXFEC | INTEGER | scalar | Count/size of cohesive crack-edge data. | XFEM/crack | xfem_control_t |
| ECRKXFETG | INTEGER | scalar | Count/size of thick-shell crack-edge data. | XFEM/crack | xfem_control_t |
| NLEVMAX | INTEGER | scalar | Maximum XFEM level-set level. | XFEM/crack | xfem_control_t |
| NUMEDGES | INTEGER | scalar | Number of crack edges. | XFEM/crack | xfem_control_t |
| NXLAYMAX | INTEGER | scalar | Maximum XFEM layer count. | XFEM/crack | xfem_control_t |
| IENRNOD | INTEGER | scalar | Enriched-node flag/count. | XFEM/crack | xfem_control_t |
| NXFENODG | INTEGER | scalar | Number of global XFEM enriched nodes. | XFEM/crack | xfem_control_t |
| NLEVXFE | INTEGER | scalar | XFEM level count. | XFEM/crack | xfem_control_t |
| NUMELCRK2 | INTEGER | scalar | Secondary crack-element count. | XFEM/crack | xfem_control_t |

### CMDLINE — Command-line option flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/commandline.inc`
**Present in**: both (starter content differs)
**Usage count**: 12 (observed) source include-sites
**Note**: Engine variant is deliberately small; the include itself warns that it is used in licensing-sensitive code.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| GOT_NTH | INTEGER | scalar | Command-line flag: thread count specified. | startup and CLI parsing | commandline_opts_t |
| NTH | INTEGER | scalar | Requested thread count. | startup and CLI parsing | commandline_opts_t |
| GOT_MEM_MAP | INTEGER | scalar | Command-line flag: memory-map option specified. | startup and CLI parsing | commandline_opts_t |
| GOT_INSPIRE | INTEGER | scalar | Command-line flag: Inspire mode specified. | startup and CLI parsing | commandline_opts_t |
| GOT_INSPIRE_ALM | INTEGER | scalar | Command-line flag: Inspire-ALM mode specified. | startup and CLI parsing | commandline_opts_t |

### SCR01 — Task-private engine task id / starter memory-buffer lengths
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr01_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 42 source include-sites
**Note**: Declared via `task_common`; this is OpenMP thread-private COMMON storage.
**Note**: `task_common` macro is used here as well; `ITASKP1` is a thread-private task identifier used in `resol_head.F` and traceback/error reporting.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ITASKP1 | INTEGER | scalar | Thread-private task identifier. | parallel/runtime | parallel_sync_t |

### SCR02 — Current element scratch identifiers
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr02_c.inc`
**Present in**: both (same block names and member order)
**Usage count**: 387 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NELTS | INTEGER | scalar | Current element-set/element-table count. | element processing | element_work_t |
| ITYPTS | INTEGER | scalar | Current element type id. | element processing | element_work_t |
| NODADT | INTEGER | scalar | Node-adjacency or node-address pointer. | time integration and stability | time_control_t |

### SCR03 — Versioning, preprocessing, and ALE/link metadata
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr03_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 958 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NRLINK | INTEGER | scalar | Number of rigid links. | preprocessing and versioning | simulation_control_t |
| NALELK | INTEGER | scalar | Number of ALE-linked entities. | ALE | ale_control_t |
| NCRST | INTEGER | scalar | Restart counter/state. | preprocessing and versioning | simulation_control_t |
| IOUTV | INTEGER | scalar | Output-version flag. | preprocessing and versioning | simulation_control_t |
| INVERS | INTEGER | scalar | Version id. | preprocessing and versioning | simulation_control_t |
| CODVERS | INTEGER | scalar | Code version number. | preprocessing and versioning | simulation_control_t |
| NOTUSED | INTEGER | scalar | Reserved field. | preprocessing and versioning | simulation_control_t |
| ICODRUN | INTEGER | scalar | Run code identifier. | preprocessing and versioning | simulation_control_t |
| IMINVER | INTEGER | scalar | Minimum compatible version. | preprocessing and versioning | simulation_control_t |
| ISRCVER | INTEGER | scalar | Source version id. | preprocessing and versioning | simulation_control_t |
| PCODVER | INTEGER | scalar | Printed/packed code version. | preprocessing and versioning | simulation_control_t |
| PMINVER | INTEGER | scalar | Printed/packed minimum version. | preprocessing and versioning | simulation_control_t |
| PSRCVER | INTEGER | scalar | Printed/packed source version. | preprocessing and versioning | simulation_control_t |
| PINVERS | INTEGER | scalar | Printed/packed input version. | preprocessing and versioning | simulation_control_t |
| PIRUN | INTEGER | scalar | Printed/packed run id. | preprocessing and versioning | simulation_control_t |
| PCODRUN | INTEGER | scalar | Legacy common-block field `PCODRUN`; purpose inferred mainly from naming. | preprocessing and versioning | simulation_control_t |
| IISROT | INTEGER | scalar | Rotation-system flag. | preprocessing and versioning | simulation_control_t |
| NXREF | INTEGER | scalar | Reference xref count. | preprocessing and versioning | simulation_control_t |
| NALELINK | INTEGER | scalar | Number of ALE links. | ALE | ale_control_t |
| ST_INVERS | INTEGER | scalar | String/packed starter version. | preprocessing and versioning | simulation_control_t |
| PDEL | INTEGER | scalar | Deletion/patch level. | preprocessing and versioning | simulation_control_t |

### SCR04 — Thread-private relation counters / starter scratch unit
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr04_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 17 source include-sites
> **⚠️ THREAD-PRIVATE** — This block carries `!$OMP THREADPRIVATE(/SCR04/)`. Every OpenMP
> thread has its own independent copy. **Do not replace with a plain shared UDT variable.**
> See the [Thread-Private Common Blocks](#️-thread-private-common-blocks--critical-refactoring-hazard) section for refactoring options.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NRTM | INTEGER | scalar | Task-local master count/index. | thread-private scratch | task_private_t |
| NRTS | INTEGER | scalar | Task-local slave/secondary count/index. | thread-private scratch | task_private_t |
| NMN | INTEGER | scalar | Task-local node master count. | thread-private scratch | task_private_t |
| NSN | INTEGER | scalar | Task-local node slave count. | thread-private scratch | task_private_t |
| NTY | INTEGER | scalar | Task-local type count/index. | thread-private scratch | task_private_t |
| NST | INTEGER | scalar | Task-local state count. | thread-private scratch | task_private_t |
| MST | INTEGER | scalar | Task-local mapping/state table index. | thread-private scratch | task_private_t |
| NLINM | INTEGER | scalar | Task-local master-line count. | thread-private scratch | task_private_t |
| NLINS | INTEGER | scalar | Task-local slave-line count. | thread-private scratch | task_private_t |
| NLINMA | INTEGER | scalar | Task-local active master-line count. | thread-private scratch | task_private_t |
| NLINSA | INTEGER | scalar | Task-local active slave-line count. | thread-private scratch | task_private_t |
| NMNE | INTEGER | scalar | Task-local master-node edge count. | thread-private scratch | task_private_t |
| NSNE | INTEGER | scalar | Task-local slave-node edge count. | thread-private scratch | task_private_t |

### SCR05 — Format, architecture, and output-control switches
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr05_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 927 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ICRAY | INTEGER | scalar | CRAY/legacy binary format flag. | I/O formats and output configuration | output_control_t |
| IRFORM | INTEGER | scalar | Restart file format flag. | I/O formats and output configuration | output_control_t |
| ITFORM | INTEGER | scalar | Time-history/output file format flag. | I/O formats and output configuration | output_control_t |
| TH_VERS | INTEGER | scalar | Time-history format version. | animation/output | output_control_t |
| IRESP | INTEGER | scalar | Response/output request flag. | I/O formats and output configuration | output_control_t |
| AFORM | INTEGER | (9) | Per-output file format codes. | I/O formats and output configuration | output_control_t |
| NGLOBTH | INTEGER | scalar | Number of global TH requests. | I/O formats and output configuration | output_control_t |
| NGLOBABF | INTEGER | scalar | Number of global ABF requests. | animation/output | output_control_t |

### SCR06 — Damping/energy scratch scalars and report flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr06_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 376 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| INERI | INTEGER | scalar | Inertia option flag. | reporting and damping | simulation_control_t |
| IPLAST | INTEGER | scalar | Plasticity option flag. | reporting and damping | simulation_control_t |
| IOFC0 | INTEGER | scalar | Offset/OFC option flag. | reporting and damping | simulation_control_t |
| ISHF | INTEGER | scalar | Shell formulation flag. | reporting and damping | simulation_control_t |
| NCINP | INTEGER | scalar | Input-cycle count. | reporting and damping | simulation_control_t |
| IREPORT | INTEGER | scalar | Report output flag. | reporting and damping | simulation_control_t |
| IH3DREADER | INTEGER | scalar | H3D reader mode flag. | animation/output | output_control_t |
| IHTML | INTEGER | scalar | HTML report flag. | reporting and damping | simulation_control_t |
| IMVW | INTEGER | scalar | HyperView/MVW output flag. | reporting and damping | simulation_control_t |
| IMVW_REF | INTEGER | scalar | Reference MVW output flag. | reporting and damping | simulation_control_t |
| NINTSKIDOLD | INTEGER | scalar | Legacy interface-skid counter. | reporting and damping | simulation_control_t |

### SCR06R — Real-valued damping/energy scratch scalars
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr06_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 376 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| HVISC | REAL(WP) | scalar | Viscous damping/energy scale. | reporting and damping | simulation_control_t |
| HELAS | REAL(WP) | scalar | Elastic energy scale. | reporting and damping | simulation_control_t |
| HVLIN | REAL(WP) | scalar | Linear-viscous coefficient. | reporting and damping | simulation_control_t |
| DT12S | REAL(WP) | scalar | Half-step shell/output increment. | time integration and stability | time_control_t |
| DMTMXS | REAL(WP) | scalar | Maximum translational momentum scale (shell). | reporting and damping | simulation_control_t |
| DMNMXS | REAL(WP) | scalar | Minimum translational momentum scale (shell). | reporting and damping | simulation_control_t |
| DEMXS | REAL(WP) | scalar | Maximum energy scale (shell). | reporting and damping | simulation_control_t |
| DMTMXK | REAL(WP) | scalar | Maximum translational momentum scale (kinematic). | reporting and damping | simulation_control_t |
| DMNMXK | REAL(WP) | scalar | Minimum translational momentum scale (kinematic). | reporting and damping | simulation_control_t |
| DEMXK | REAL(WP) | scalar | Maximum energy scale (kinematic). | reporting and damping | simulation_control_t |

### SCR07 — File units and run-mode flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr07_c.inc`
**Present in**: engine only
**Usage count**: 643 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IFREEF | INTEGER | scalar | Free-format input flag. | file and run control | file_io_ctrl_t |
| MDESS | INTEGER | scalar | Scratch descriptor file unit. | file and run control | file_io_ctrl_t |
| MREST | INTEGER | scalar | Restart file unit. | file and run control | file_io_ctrl_t |
| MSTOP | INTEGER | scalar | Stop file/control unit. | time integration and stability | time_control_t |
| MANIM | INTEGER | scalar | Animation file unit. | animation/output | output_control_t |
| IFOP | INTEGER | scalar | Output file open flag. | file and run control | file_io_ctrl_t |
| ITOP | INTEGER | scalar | Output type selector. | file and run control | file_io_ctrl_t |
| LLLINK | INTEGER | scalar | Length of link scratch area. | file and run control | file_io_ctrl_t |
| LLINAL | INTEGER | scalar | Length of line scratch area. | file and run control | file_io_ctrl_t |
| RAD_IPFAIL | INTEGER | scalar | Radioss failure-print flag. | file and run control | file_io_ctrl_t |
| MCHECK | INTEGER | scalar | Input-check mode flag. | file and run control | file_io_ctrl_t |
| NTH | INTEGER | scalar | Requested thread count. | file and run control | file_io_ctrl_t |
| NANIM | INTEGER | scalar | Animation file count. | animation/output | output_control_t |
| IRPREV | INTEGER | scalar | Previous-run/restart index. | file and run control | file_io_ctrl_t |
| WMCHECK | INTEGER | scalar | Write-check mode flag. | curves/tables/integration | registry_tables_t |
| NABFILE | INTEGER | scalar | Number of ABF files. | animation/output | output_control_t |
| ICTLSTOP | INTEGER | scalar | External stop-control flag. | time integration and stability | time_control_t |
| NERR_POSIT | INTEGER | scalar | Positive-orientation error count. | file and run control | file_io_ctrl_t |
| NSDYNANIN | INTEGER | scalar | DYNAIN input-set count. | animation/output | output_control_t |

### SCR08_A — Quadrilateral local coordinate scratch
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr08_a_c.inc`
**Present in**: engine only
**Usage count**: 45 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| XX1 | REAL(WP) | (4) | Local coordinate x-components for corner 1 set. | element geometry | element_work_t |
| XX2 | REAL(WP) | (4) | Local coordinate x-components for corner 2 set. | element geometry | element_work_t |
| XX3 | REAL(WP) | (4) | Local coordinate x-components for corner 3 set. | element geometry | element_work_t |
| IX | INTEGER | (4) | Local corner/node index array X-side. | element geometry | element_work_t |
| IY | INTEGER | (4) | Local corner/node index array Y-side. | element geometry | element_work_t |

### SCR11 — Energy/momentum balance or starter shell-mass scratch
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr11_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 270 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ENINT | REAL(WP) | scalar | Internal energy total. | energy accounting | energy_balance_t |
| ENCIN | REAL(WP) | scalar | Kinetic energy total. | energy accounting | energy_balance_t |
| XMOMT | REAL(WP) | scalar | Total X momentum. | time integration and stability | time_control_t |
| YMOMT | REAL(WP) | scalar | Total Y momentum. | time integration and stability | time_control_t |
| ZMOMT | REAL(WP) | scalar | Total Z momentum. | time integration and stability | time_control_t |
| XMASS | REAL(WP) | scalar | Total mass. | time integration and stability | time_control_t |
| ENROT | REAL(WP) | scalar | Rotational energy total. | energy accounting | energy_balance_t |
| ECONT | REAL(WP) | scalar | Contact energy total. | energy accounting | energy_balance_t |
| MASS0 | REAL(WP) | scalar | Initial total mass. | time integration and stability | time_control_t |
| ENTOT0 | REAL(WP) | scalar | Initial total energy. | energy accounting | energy_balance_t |
| ENCIN2 | REAL(WP) | scalar | Secondary kinetic-energy accumulator. | energy accounting | energy_balance_t |
| ENROT2 | REAL(WP) | scalar | Secondary rotational-energy accumulator. | energy accounting | energy_balance_t |
| DELTAE | REAL(WP) | scalar | Energy balance residual. | energy accounting | energy_balance_t |
| EAMS | REAL(WP) | scalar | Added-mass/stabilization energy. | energy accounting | energy_balance_t |
| WPLAST | REAL(WP) | scalar | Plastic work total. | energy accounting | energy_balance_t |

### SCR12 — Reaction/export scratch flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr12_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 206 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| CPTREAC | INTEGER | scalar | Reaction accumulation counter. | reactions / decomposition | simulation_control_t |

### SCR13 — Scratch file logical unit
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr13_c.inc`
**Present in**: engine only
**Usage count**: 89 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IUNIT | INTEGER | scalar | Scratch logical unit number. | file and unit handling | file_io_ctrl_t |

### SCR14 — Animation variable selection lists and flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr14_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 1180 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NV_ANI | INTEGER | scalar | Count of entries in the V_ANI request list. | animation output | output_control_t |
| ANIM_V | INTEGER | (MX_ANI) | Requested animation selector(s) for `V`. | animation output | output_control_t |
| NT_ANI | INTEGER | scalar | Count of entries in the T_ANI request list. | animation output | output_control_t |
| ANIM_T | INTEGER | (MX_ANI) | Requested animation selector(s) for `T`. | animation output | output_control_t |
| NE_ANI | INTEGER | scalar | Count of entries in the E_ANI request list. | animation output | output_control_t |
| ANIM_E | INTEGER | (MX_ANI) | Requested animation selector(s) for `E`. | animation output | output_control_t |
| NN_ANI | INTEGER | scalar | Count of entries in the N_ANI request list. | animation output | output_control_t |
| ANIM_N | INTEGER | (MX_ANI) | Requested animation selector(s) for `N`. | animation output | output_control_t |
| NCT_ANI | INTEGER | scalar | Count of entries in the CT_ANI request list. | animation output | output_control_t |
| ANIM_CT | INTEGER | (MX_ANI) | Requested animation selector(s) for `CT`. | animation output | output_control_t |
| NCE_ANI | INTEGER | scalar | Count of entries in the CE_ANI request list. | animation output | output_control_t |
| ANIM_CE | INTEGER | (MX_ANI) | Requested animation selector(s) for `CE`. | animation output | output_control_t |
| NST_ANI | INTEGER | scalar | Count of entries in the ST_ANI request list. | animation output | output_control_t |
| ANIM_ST | INTEGER | (MX_ANI) | Requested animation selector(s) for `ST`. | animation output | output_control_t |
| NSE_ANI | INTEGER | scalar | Count of entries in the SE_ANI request list. | animation output | output_control_t |
| ANIM_SE | INTEGER | (MX_ANI) | Requested animation selector(s) for `SE`. | animation output | output_control_t |
| NFT_ANI | INTEGER | scalar | Count of entries in the FT_ANI request list. | animation output | output_control_t |
| ANIM_FT | INTEGER | (MX_ANI) | Requested animation selector(s) for `FT`. | animation output | output_control_t |
| NFE_ANI | INTEGER | scalar | Count of entries in the FE_ANI request list. | animation output | output_control_t |
| ANIM_FE | INTEGER | (MX_ANI) | Requested animation selector(s) for `FE`. | animation output | output_control_t |
| ANIM_M | INTEGER | scalar | Requested animation selector(s) for `M`. | animation output | output_control_t |
| ANIM_K | INTEGER | scalar | Requested animation selector(s) for `K`. | animation output | output_control_t |
| ANIM_U | INTEGER | scalar | Requested animation selector(s) for `U`. | animation output | output_control_t |
| ANIM_MAT | INTEGER | scalar | Requested animation selector(s) for `MAT`. | animation output | output_control_t |
| IEPSDOT | INTEGER | scalar | Integer flag/index for `EPSDOT`. | animation output | output_control_t |
| FMT_ANI | INTEGER | scalar | Legacy common-block field `FMT_ANI`; purpose inferred mainly from naming. | animation output | output_control_t |
| ANIM_VERS | INTEGER | scalar | Requested animation selector(s) for `VERS`. | animation output | output_control_t |
| IZIP | INTEGER | scalar | Integer flag/index for `ZIP`. | animation output | output_control_t |
| IFVANI | INTEGER | scalar | Integer flag/index for `FVANI`. | animation output | output_control_t |
| IFAILA | INTEGER | scalar | Integer flag/index for `FAILA`. | animation output | output_control_t |
| IAD_GPS | INTEGER | scalar | Integer flag/index for `AD_GPS`. | animation output | output_control_t |
| IZIPSTRS | INTEGER | scalar | Integer flag/index for `ZIPSTRS`. | animation output | output_control_t |
| ANIM_PLY | INTEGER | scalar | Requested animation selector(s) for `PLY`. | animation output | output_control_t |
| ANIM_CRK | INTEGER | scalar | Requested animation selector(s) for `CRK`. | animation output | output_control_t |
| TITLETAB | CHARACTER*80 | (MX_ANI) | Time/state value for `ITLETAB`. | animation output | output_control_t |
| NTITLETAB | INTEGER | (MX_ANI) | Count/flag for `TITLETAB`. | animation output | output_control_t |
| NLTITLE | INTEGER | scalar | Count/flag for `LTITLE`. | animation output | output_control_t |
| ISTRESALL | INTEGER | scalar | Integer flag/index for `STRESALL`. | animation output | output_control_t |
| ISTRAIALL | INTEGER | scalar | Integer flag/index for `STRAIALL`. | animation output | output_control_t |
| IEPSDOALL | INTEGER | scalar | Integer flag/index for `EPSDOALL`. | animation output | output_control_t |
| IEPSPALL | INTEGER | scalar | Integer flag/index for `EPSPALL`. | animation output | output_control_t |
| IEPSPNLALL | INTEGER | scalar | Integer flag/index for `EPSPNLALL`. | animation output | output_control_t |
| IORTHDALL | INTEGER | scalar | Integer flag/index for `ORTHDALL`. | animation output | output_control_t |
| IEPSPFULL | INTEGER | scalar | Integer flag/index for `EPSPFULL`. | animation output | output_control_t |
| ISTRESFULL | INTEGER | scalar | Integer flag/index for `STRESFULL`. | animation output | output_control_t |
| IPLYALL | INTEGER | scalar | Integer flag/index for `PLYALL`. | animation output | output_control_t |
| ISTRESALL_PLY | INTEGER | scalar | Integer flag/index for `STRESALL_PLY`. | animation output | output_control_t |
| ISTRESALL_PLY_IPT | INTEGER | scalar | Integer flag/index for `STRESALL_PLY_IPT`. | animation output | output_control_t |
| IPHIALL_PLY | INTEGER | scalar | Integer flag/index for `PHIALL_PLY`. | animation output | output_control_t |
| IEPSPALL_PLY | INTEGER | scalar | Integer flag/index for `EPSPALL_PLY`. | animation output | output_control_t |
| ISTRAINALL_PLY | INTEGER | scalar | Integer flag/index for `STRAINALL_PLY`. | animation output | output_control_t |
| IDAMAFULL | INTEGER | scalar | Integer flag/index for `DAMAFULL`. | animation output | output_control_t |
| ISTRAINFULL | INTEGER | scalar | Integer flag/index for `STRAINFULL`. | animation output | output_control_t |
| IEPSDOFULL | INTEGER | scalar | Integer flag/index for `EPSDOFULL`. | animation output | output_control_t |
| IPHIALL | INTEGER | scalar | Integer flag/index for `PHIALL`. | animation output | output_control_t |
| IWPLAFULL | INTEGER | scalar | Integer flag/index for `WPLAFULL`. | animation output | output_control_t |
| IWPLAALL | INTEGER | scalar | Integer flag/index for `WPLAALL`. | animation output | output_control_t |
| IDAMAALL | INTEGER | scalar | Integer flag/index for `DAMAALL`. | animation output | output_control_t |
| IEPSDNLALL | INTEGER | scalar | Integer flag/index for `EPSDNLALL`. | animation output | output_control_t |
| INXTFALL | INTEGER | scalar | Integer flag/index for `NXTFALL`. | animation output | output_control_t |
| SIGH1ALL | INTEGER | scalar | Legacy common-block field `SIGH1ALL`; purpose inferred mainly from naming. | animation output | output_control_t |
| SIGH2ALL | INTEGER | scalar | Legacy common-block field `SIGH2ALL`; purpose inferred mainly from naming. | animation output | output_control_t |
| ITSAIWUALL | INTEGER | scalar | Integer flag/index for `TSAIWUALL`. | animation output | output_control_t |
| ITSAIWUFULL | INTEGER | scalar | Integer flag/index for `TSAIWUFULL`. | animation output | output_control_t |

### SCR16R — State-output time base
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr16_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 956 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| DTOUTP0 | REAL(WP) | scalar | Reference/base output interval. | state output scheduling | output_control_t |
| TOUTP0 | REAL(WP) | scalar | Reference/base next-output time. | state output scheduling | output_control_t |

### SCR16 — State-output selection lists
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr16_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 956 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NV_OUTP | INTEGER | scalar | Count of entries in the V_OUTP request list. | state output selection | output_control_t |
| OUTP_V | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `V`. | state output selection | output_control_t |
| NN_OUTP | INTEGER | scalar | Count of entries in the N_OUTP request list. | state output selection | output_control_t |
| OUTP_N | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `N`. | state output selection | output_control_t |
| NSS_OUTP | INTEGER | scalar | Count of entries in the SS_OUTP request list. | state output selection | output_control_t |
| OUTP_SS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `SS`. | state output selection | output_control_t |
| NCS_OUTP | INTEGER | scalar | Count of entries in the CS_OUTP request list. | state output selection | output_control_t |
| OUTP_CS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `CS`. | state output selection | output_control_t |
| NTS_OUTP | INTEGER | scalar | Count of entries in the TS_OUTP request list. | state output selection | output_control_t |
| OUTP_TS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `TS`. | state output selection | output_control_t |
| NPS_OUTP | INTEGER | scalar | Count of entries in the PS_OUTP request list. | state output selection | output_control_t |
| OUTP_PS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `PS`. | state output selection | output_control_t |
| NRS_OUTP | INTEGER | scalar | Count of entries in the RS_OUTP request list. | state output selection | output_control_t |
| OUTP_RS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `RS`. | state output selection | output_control_t |
| NST_OUTP | INTEGER | scalar | Count of entries in the ST_OUTP request list. | state output selection | output_control_t |
| OUTP_ST | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `ST`. | state output selection | output_control_t |
| NCT_OUTP | INTEGER | scalar | Count of entries in the CT_OUTP request list. | state output selection | output_control_t |
| OUTP_CT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `CT`. | state output selection | output_control_t |
| NTT_OUTP | INTEGER | scalar | Count of entries in the TT_OUTP request list. | state output selection | output_control_t |
| OUTP_TT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `TT`. | state output selection | output_control_t |
| NPT_OUTP | INTEGER | scalar | Count of entries in the PT_OUTP request list. | state output selection | output_control_t |
| OUTP_PT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `PT`. | state output selection | output_control_t |
| NRT_OUTP | INTEGER | scalar | Count of entries in the RT_OUTP request list. | state output selection | output_control_t |
| OUTP_RT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `RT`. | state output selection | output_control_t |
| IOUTP_FMT | INTEGER | scalar | Integer flag/index for `OUTP_FMT`. | state output selection | output_control_t |
| OUTYY_FMT | INTEGER | scalar | Legacy common-block field `OUTYY_FMT`; purpose inferred mainly from naming. | state output selection | output_control_t |
| NSPS_OUTP | INTEGER | scalar | Count of entries in the SPS_OUTP request list. | state output selection | output_control_t |
| OUTP_SPS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `SPS`. | state output selection | output_control_t |
| NSPT_OUTP | INTEGER | scalar | Count of entries in the SPT_OUTP request list. | state output selection | output_control_t |
| OUTP_SPT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `SPT`. | state output selection | output_control_t |
| IROOTYY | INTEGER | scalar | YY/root output flag. | state output selection | output_control_t |
| IDROT | INTEGER | scalar | Rotational output flag. | state output selection | output_control_t |
| MOUTPT | INTEGER | scalar | Output table length. | state output selection | output_control_t |
| MOUTP | INTEGER | (MX_OUTP2) | Packed output selector table. | state output selection | output_control_t |

### SCR16_STATR — Status-print time base
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr16_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 956 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| DTSTAT0 | REAL(WP) | scalar | Reference/base status interval. | status output scheduling | output_control_t |
| TSTAT0 | REAL(WP) | scalar | Reference/base next-status time. | status output scheduling | output_control_t |
| DTABF0 | REAL(WP) | (10) | Reference/base ABF intervals (10 slots). | status output scheduling | output_control_t |
| DTABFWR0 | REAL(WP) | (10) | Reference/base ABF write intervals (10 slots). | status output scheduling | output_control_t |

### SCR16_STATI — Status-print entity selections
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr16_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 956 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| MX_STAT0 | INTEGER | scalar | Requested number of status channels. | status output selection | output_control_t |
| NSTATPRT | INTEGER | scalar | Number of printed status entries. | status output selection | output_control_t |
| NC_STAT | INTEGER | scalar | Number of status curves/sets. | status output selection | output_control_t |
| STAT_C | INTEGER | (MX_STAT) | Legacy common-block field `STAT_C`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELC | INTEGER | scalar | Legacy common-block field `STAT_NUMELC`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELTG | INTEGER | scalar | Legacy common-block field `STAT_NUMELTG`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELC_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELC_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELTG_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELTG_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_S | INTEGER | (MX_STAT) | Legacy common-block field `STAT_S`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELS | INTEGER | scalar | Legacy common-block field `STAT_NUMELS`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELQ | INTEGER | scalar | Legacy common-block field `STAT_NUMELQ`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELT | INTEGER | scalar | Legacy common-block field `STAT_NUMELT`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELS_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELS_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELQ_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELQ_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELT_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELT_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_N | INTEGER | (MX_STAT) | Legacy common-block field `STAT_N`; purpose inferred mainly from naming. | status output selection | output_control_t |
| MSTATT | INTEGER | scalar | Status table length. | status output selection | output_control_t |
| MSTAT | INTEGER | (MX_STAT2) | Packed map/buffer for `STAT`. | status output selection | output_control_t |
| STAT_NUMELR | INTEGER | scalar | Legacy common-block field `STAT_NUMELR`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELR_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELR_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_R | INTEGER | (MX_STAT) | Legacy common-block field `STAT_R`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELP | INTEGER | scalar | Legacy common-block field `STAT_NUMELP`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELP_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELP_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_P | INTEGER | (MX_STAT) | Legacy common-block field `STAT_P`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_T | INTEGER | (MX_STAT) | Legacy common-block field `STAT_T`; purpose inferred mainly from naming. | status output selection | output_control_t |
| NSTATALL | INTEGER | scalar | Print all status variables flag/count. | status output selection | output_control_t |
| STAT_INIMAP | INTEGER | (MX_STAT3) | Status initial-mapping selectors. | status output selection | output_control_t |

### SCR17 — Deletion/off-state counters and id-buffer lengths
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr17_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 3180 source include-sites
**Note**: Engine extends the starter id-length block with actual deletion/off-state counters (`IDEL7`, `NSOLOFF`, `NSH4OFF`, ...) used by element-deletion and restart logic.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IDEL7 | INTEGER | scalar | Deletion enabled flag for interface type 7. | element deletion and id tables | deletion_control_t |
| IDT1SH | INTEGER | scalar | Time-step/deletion control for shell family. | element deletion and id tables | deletion_control_t |
| IDEL7NG | INTEGER | scalar | Deleted type-7 count (NG side). | element deletion and id tables | deletion_control_t |
| IDEL7NOK | INTEGER | scalar | Non-OK type-7 deletion count. | element deletion and id tables | deletion_control_t |
| IDEL2NG | INTEGER | scalar | Deleted type-2 count. | element deletion and id tables | deletion_control_t |
| NSOLOFF | INTEGER | scalar | Number of solid elements switched off. | element deletion and id tables | deletion_control_t |
| NSH4OFF | INTEGER | scalar | Number of 4-node shells switched off. | element deletion and id tables | deletion_control_t |
| NSH3OFF | INTEGER | scalar | Number of 3-node shells switched off. | element deletion and id tables | deletion_control_t |
| NTRSOFF | INTEGER | scalar | Number of trusses switched off. | element deletion and id tables | deletion_control_t |
| NPOUOFF | INTEGER | scalar | Number of beams/rods switched off. | element deletion and id tables | deletion_control_t |
| NSPROFF | INTEGER | scalar | Number of springs switched off. | element deletion and id tables | deletion_control_t |
| LCNEL | INTEGER | scalar | Length/index of deleted element connectivity. | element deletion and id tables | deletion_control_t |
| ISMOOTH4 | INTEGER | scalar | 4-node shell smoothing option. | element deletion and id tables | deletion_control_t |
| LIDFS | INTEGER | scalar | Length/index for interface-deletion flags. | element deletion and id tables | deletion_control_t |
| LIRNUR | INTEGER | scalar | Length/index for renumbering buffer. | element deletion and id tables | deletion_control_t |
| IDTS6 | INTEGER | scalar | Time-step/deletion control for 6-node shell family. | element deletion and id tables | deletion_control_t |
| LNOPT1 | INTEGER | scalar | Length/index of option-1 table. | element deletion and id tables | deletion_control_t |
| LTITR | INTEGER | scalar | Length/index of title table. | element deletion and id tables | deletion_control_t |
| LILSET1 | INTEGER | scalar | Length/index of level-set table 1. | element deletion and id tables | deletion_control_t |
| LISLIN1 | INTEGER | scalar | Length/index of line-set table 1. | element deletion and id tables | deletion_control_t |
| LISUB1 | INTEGER | scalar | Length/index of substructure table 1. | element deletion and id tables | deletion_control_t |
| LISURF1 | INTEGER | scalar | Length/index of surface table 1. | element deletion and id tables | deletion_control_t |
| LIPART1 | INTEGER | scalar | Length/index of part table 1. | element deletion and id tables | deletion_control_t |
| LIGRN1 | INTEGER | scalar | Length/index of group-node table 1. | element deletion and id tables | deletion_control_t |
| IDT1SOL | INTEGER | scalar | Time-step/deletion control for solid family. | element deletion and id tables | deletion_control_t |
| INEG_V | INTEGER | scalar | Negative-volume detection flag/count. | element deletion and id tables | deletion_control_t |
| IDT1TET10 | INTEGER | scalar | Time-step/deletion control for tet10 family. | element deletion and id tables | deletion_control_t |
| IDTTSH | INTEGER | scalar | Time-step/deletion control for thick-shell family. | element deletion and id tables | deletion_control_t |

### SCR18R — Per-group timestep controls and thresholds
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr18_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 1087 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| DTFAC1 | REAL(WP) | (102) | Per-group time-step scale factors (102 slots). | time-step control | timestep_dt_t |
| DTMIN1 | REAL(WP) | (102) | Per-group minimum time steps (102 slots). | time-step control | timestep_dt_t |
| PERCENT_ADDMASS | REAL(WP) | scalar | Allowed added-mass percentage. | time-step control | timestep_dt_t |
| DT_STOP_PERCENT_ADDMASS | REAL(WP) | scalar | Stop threshold for added-mass percentage. | time-step control | timestep_dt_t |
| MASS0_START | REAL(WP) | scalar | Initial mass at start of added-mass monitoring. | time-step control | timestep_dt_t |
| PERCENT_ADDMASS_OLD | REAL(WP) | scalar | Previous added-mass percentage. | time-step control | timestep_dt_t |

### SCR18 — Per-group timestep mode flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr18_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 1087 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IDTMIN | INTEGER | (102) | Per-group time-step enforcement modes (102 slots). | time-step control | timestep_dt_t |
| IDTGR | INTEGER | (102) | Per-group time-step group ids (102 slots). | time-step control | timestep_dt_t |
| KDTINT | INTEGER | scalar | Global time-step integration mode. | time-step control | timestep_dt_t |
| KDTSMSTR | INTEGER | scalar | Master/subcycling time-step mode. | time-step control | timestep_dt_t |
| IDT_PERCENT_ADDMASS | INTEGER | scalar | Added-mass percentage control flag. | time-step control | timestep_dt_t |

### SCR19 — Registry offsets for curves/tables/materials/properties
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr19_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 126 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| JFUNC | INTEGER | (MAXFUNC) | Function registry offsets. | curves/tables/materials/properties | registry_tables_t |
| JTAB | INTEGER | (MAXTAB) | Table registry offsets. | curves/tables/materials/properties | registry_tables_t |
| NJFUNC | INTEGER | scalar | Number of registered functions. | curves/tables/materials/properties | registry_tables_t |
| NJTAB | INTEGER | scalar | Number of registered tables. | curves/tables/materials/properties | registry_tables_t |

### SCR19R — Geometry scratch buffer
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr19_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 126 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| BUFGEO0 | REAL(WP) | (BGEOSIZE) | Geometry scratch buffer. | geometry scratch | registry_tables_t |

### SCR20 — ABF pipe I/O handles
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr20_c.inc`
**Present in**: engine only
**Usage count**: 35 source include-sites
**Note**: Used by ABF/pipe startup and shutdown (`OPEN_ABFPIPE`, `CHECK_ABF`, `RELEASE_ABFPIPE`).

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IABFPIPE | INTEGER | scalar | Status/handle for ABF pipe connection. | animation/output | output_control_t |
| ABINP | INTEGER | scalar | ABF input file descriptor/unit. | ABF/file piping | file_io_ctrl_t |
| ABOUT | INTEGER | scalar | ABF output file descriptor/unit. | ABF/file piping | file_io_ctrl_t |

### SCR23 — 2D/3D animation counters
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr23_c.inc`
**Present in**: both (same block names and member order)
**Usage count**: 262 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NIXX | INTEGER | scalar | Current NX-sized animation/work index. | animation output | output_control_t |
| MAXNX | INTEGER | scalar | Maximum NX-sized animation/work index. | animation output | output_control_t |
| NANIM2D | INTEGER | scalar | 2D animation count. | animation/output | output_control_t |
| NANIM3D | INTEGER | scalar | 3D animation count. | animation/output | output_control_t |

### ANIVAR1 — Per-ply effective plastic strain animation flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr25_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_EPSP | INTEGER | (MAXLAY) | Per-ply effective plastic strain animation selector array. | composite animation | animation_layers_t |

### ANIVAR2 — Per-ply stress animation flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr25_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_STRESS | INTEGER | (MAXLAY) | Per-ply stress animation selector array. | composite animation | animation_layers_t |

### ANIVAR3 — Per-ply damage animation flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr25_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_DAMA | INTEGER | (MAXLAY) | Per-ply damage animation selector array. | composite animation | animation_layers_t |

### ANIVAR4 — Per-ply strain animation flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr25_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_STRAIN | INTEGER | (MAXLAY) | Per-ply strain animation selector array. | composite animation | animation_layers_t |

### ANIVAR5 — Per-ply strain-rate animation flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr25_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_EPSDOT | INTEGER | (MAXLAY) | Per-ply strain-rate animation selector array. | composite animation | animation_layers_t |

### ANIVAR6 — Per-ply plastic work animation flags
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr25_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_WPLA | INTEGER | (MAXLAY) | Per-ply plastic-work animation selector array. | composite animation | animation_layers_t |

### SCR25 — Ply and layered animation selections
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr25_c.inc`
**Present in**: both (starter content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IEPSDOTALL_PLY | INTEGER | scalar | Output all ply strain-rate tensors flag. | composite animation | animation_layers_t |
| IDAMAALL_PLY | INTEGER | scalar | Output all ply damage values flag. | composite animation | animation_layers_t |
| PLY_ANIM | INTEGER | (MX_PLY_ANIM) | Packed list of selected ply ids. | composite animation | animation_layers_t |
| PLY_ANIM_STRESS | INTEGER | (MX_PLY_ANIM) | Packed list of ply stress outputs. | composite animation | animation_layers_t |
| PLY_ANIM_PHI | INTEGER | (MX_PLY_ANIM) | Packed list of ply failure index/phi outputs. | composite animation | animation_layers_t |
| PLY_ANIM_EPSP | INTEGER | (MX_PLY_ANIM) | Packed list of ply plastic-strain outputs. | composite animation | animation_layers_t |
| PLY_ANIM_STRAIN | INTEGER | (MX_PLY_ANIM) | Packed list of ply strain outputs. | composite animation | animation_layers_t |
| PLY_ANIM_EPSDOT | INTEGER | (MX_PLY_ANIM) | Packed list of ply strain-rate outputs. | composite animation | animation_layers_t |
| PLY_ANIM_DAMA | INTEGER | (MX_PLY_ANIM) | Packed list of ply damage outputs. | composite animation | animation_layers_t |
| NBPLY_ANIM | INTEGER | scalar | Number of selected ply ids. | composite animation | animation_layers_t |
| NBPLY_ANIM_STRESS | INTEGER | scalar | Number of selected ply stress outputs. | composite animation | animation_layers_t |
| NBPLY_ANIM_PHI | INTEGER | scalar | Number of selected ply phi outputs. | composite animation | animation_layers_t |
| NBPLY_ANIM_EPSP | INTEGER | scalar | Number of selected ply plastic-strain outputs. | composite animation | animation_layers_t |
| NBPLY_ANIM_STRAIN | INTEGER | scalar | Number of selected ply strain outputs. | composite animation | animation_layers_t |
| NBPLY_ANIM_EPSDOT | INTEGER | scalar | Number of selected ply strain-rate outputs. | composite animation | animation_layers_t |
| NBPLY_ANIM_DAMA | INTEGER | scalar | Number of selected ply damage outputs. | composite animation | animation_layers_t |
| IBRICK_STRESSALL | INTEGER | scalar | Output all brick stresses flag. | composite animation | animation_layers_t |
| IBRICK_STRAINALL | INTEGER | scalar | Output all brick strains flag. | composite animation | animation_layers_t |
| IBRICK_EPSPALL | INTEGER | scalar | Output all brick plastic strains flag. | composite animation | animation_layers_t |
| IBEAM_EPSPALL | INTEGER | scalar | Output all beam plastic strains flag. | composite animation | animation_layers_t |

### SCRFCI — Integer scratch partition offsets
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr_fac_c.inc`
**Present in**: engine only
**Usage count**: 47 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IFIF | INTEGER | scalar | Integer scratch partition length/offset for workspace family IF. | workspace sizing | workspace_partition_t |
| IF01 | INTEGER | scalar | Integer scratch partition boundary 01. | workspace sizing | workspace_partition_t |
| IF02 | INTEGER | scalar | Integer scratch partition boundary 02. | workspace sizing | workspace_partition_t |
| IF03 | INTEGER | scalar | Integer scratch partition boundary 03. | workspace sizing | workspace_partition_t |
| IF04 | INTEGER | scalar | Integer scratch partition boundary 04. | workspace sizing | workspace_partition_t |
| IF05 | INTEGER | scalar | Integer scratch partition boundary 05. | workspace sizing | workspace_partition_t |

### SCRFCR — Real scratch partition offsets
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scr_fac_c.inc`
**Present in**: engine only
**Usage count**: 47 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| MFIF | INTEGER | scalar | Real scratch partition length/offset for workspace family IF. | workspace sizing | workspace_partition_t |
| MF01 | INTEGER | scalar | Real scratch partition boundary 01. | workspace sizing | workspace_partition_t |
| MF02 | INTEGER | scalar | Real scratch partition boundary 02. | workspace sizing | workspace_partition_t |
| MF03 | INTEGER | scalar | Real scratch partition boundary 03. | workspace sizing | workspace_partition_t |
| MF04 | INTEGER | scalar | Real scratch partition boundary 04. | workspace sizing | workspace_partition_t |
| MF05 | INTEGER | scalar | Real scratch partition boundary 05. | workspace sizing | workspace_partition_t |

### SCRCUT — Section-cut count
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scrcut_c.inc`
**Present in**: both (same block names and member order)
**Usage count**: 102 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NCUTS | INTEGER | scalar | Number of section cuts. | section cuts | output_control_t |

### SCRFS — Functions and fluid-surface counts
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scrfs_c.inc`
**Present in**: engine only
**Usage count**: 83 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NFCT | INTEGER | scalar | Number of function curves. | curves and fluid surfaces | file_io_ctrl_t |
| NSFLSW | INTEGER | scalar | Number of fluid surface sets. | airbag/fluid | airbag_control_t |
| NTFLSW | INTEGER | scalar | Total number of faces in all fluid surface sets. | airbag/fluid | airbag_control_t |

### SCRNO_I — Noise-analysis integer control
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scrnoi_c.inc`
**Present in**: engine only
**Usage count**: 95 source include-sites
**Note**: `NNOISE`, `RNOI`, and `DTNOISE` drive Rad2Noise initialization and sampling in `engine/source/general_controls/computation/noise.F`.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NNOISE | INTEGER | scalar | Number of noise sensors/requests. | noise post-processing | noise_control_t |
| RNOI | INTEGER | scalar | Noise mode/restart flag. | noise post-processing | noise_control_t |
| NOISEV | INTEGER | scalar | Noise velocity output flag. | noise post-processing | noise_control_t |
| NOISEP | INTEGER | scalar | Noise pressure output flag. | noise post-processing | noise_control_t |
| NOISEA | INTEGER | scalar | Noise acceleration output flag. | noise post-processing | noise_control_t |

### SCRNO_R — Noise-analysis timing control
**File**: `/home/laurent/OpenRadioss2/engine/share/includes/scrnoi_c.inc`
**Present in**: engine only
**Usage count**: 95 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| TNOISE | REAL(WP) | scalar | Noise start/reference time. | noise post-processing | noise_control_t |
| DTNOISE | REAL(WP) | scalar | Noise sampling period. | noise post-processing | noise_control_t |

### LOCKMP — Shared lock matrix for parallel sections
**File**: `/home/laurent/OpenRadioss2/engine/share/spe_inc/comlock.inc`
**Present in**: both (same block names and member order)
**Usage count**: 4080 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| LLOCK | INTEGER | (16,2*INTSEG2) | Lock table used by threaded/MPI synchronization. | parallel synchronization | parallel_sync_t |

## Starter common blocks

Only starter-specific or content-divergent entries are repeated here; identical blocks are documented in the engine section and marked as present in both.

### COM01 — Global simulation flags, counters, and feature switches
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/com01_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 8224 source include-sites
**Note**: Starter variant is larger than engine: it adds preprocessing/layout fields such as `IMULTI_FVM`, `IUSHELL/IUSOLID`, multiple `NVSOLID*`/`NVSHELL*` counters, `KMP_*`, `IPART_PCOMPP`, and other starter-only import/configuration flags.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| N2D | INTEGER | scalar | 2D model flag. | global solver control | simulation_control_t |
| NCPRI | INTEGER | scalar | Print-control level/counter. | global solver control | simulation_control_t |
| IALE | INTEGER | scalar | ALE formulation enabled flag. | ALE | ale_control_t |
| NGROUP | INTEGER | scalar | Count/flag for `GROUP`. | loads, groups, and BCs | entity_counts_t |
| NCYCLE | INTEGER | scalar | Current explicit cycle counter. | time integration and stability | time_control_t |
| IRUN | INTEGER | scalar | Run/restart index. | global solver control | simulation_control_t |
| IGER | INTEGER | scalar | General solver mode/status flag. | global solver control | simulation_control_t |
| LBUFEL | INTEGER | scalar | Length of the element scratch/work buffer. | global solver control | simulation_control_t |
| IRODDL | INTEGER | scalar | Odd/even or rotational DOF mode flag. | global solver control | simulation_control_t |
| IEULER | INTEGER | scalar | Eulerian formulation enabled flag. | global solver control | simulation_control_t |
| IMULTI_FVM | INTEGER | scalar | Multi-fluid/FVM processing enabled flag. | global solver control | simulation_control_t |
| IHSH | INTEGER | scalar | Hourglass/shell option flag. | global solver control | simulation_control_t |
| ITESTV | INTEGER | scalar | Verification/test mode flag. | global solver control | simulation_control_t |
| ITURB | INTEGER | scalar | Turbulence option flag. | global solver control | simulation_control_t |
| ILAG | INTEGER | scalar | Lagrangian formulation flag. | global solver control | simulation_control_t |
| ISECUT | INTEGER | scalar | Section-cut feature enabled flag. | global solver control | simulation_control_t |
| IDAMP | INTEGER | scalar | Global damping option flag. | time integration and stability | time_control_t |
| IRXDP | INTEGER | scalar | Extra precision / XDP mode flag. | global solver control | simulation_control_t |
| NMULT | INTEGER | scalar | Count of multi-domain/multi-run entities. | global solver control | simulation_control_t |
| INTEG8 | INTEGER | scalar | 8-point integration flag. | global solver control | simulation_control_t |
| ISIGI | INTEGER | scalar | Stress initialization flag. | global solver control | simulation_control_t |
| NSPMD | INTEGER | scalar | SPMD domain count. | parallel/runtime | parallel_sync_t |
| LENWA | INTEGER | scalar | Length of working array WA. | global solver control | simulation_control_t |
| NNODS | INTEGER | scalar | Number of nodes per current/local stencil. | global solver control | simulation_control_t |
| NCNOIS | INTEGER | scalar | Noise variable count per sensor. | global solver control | simulation_control_t |
| LCNE0 | INTEGER | scalar | Length of node-to-element connectivity tables. | global solver control | simulation_control_t |
| IPARI0 | INTEGER | scalar | Base offset/index into IPARI tables. | parallel/runtime | parallel_sync_t |
| IMAXIMP | INTEGER | scalar | Maximum implicit-iteration or imposed-count setting. | global solver control | simulation_control_t |
| NNOISER | INTEGER | scalar | Restart copy of noise request count. | noise post-processing | noise_control_t |
| NSPGROUP | INTEGER | scalar | Number of SPMD groups. | loads, groups, and BCs | entity_counts_t |
| IRESMD | INTEGER | scalar | Restart/resume mode flag. | global solver control | simulation_control_t |
| IFRWV | INTEGER | scalar | Wave/restart-output flag. | global solver control | simulation_control_t |
| INTBAG | INTEGER | scalar | Airbag interface/model flag. | airbag/fluid | airbag_control_t |
| ICLOSE | INTEGER | scalar | Closure/contact closing flag. | global solver control | simulation_control_t |
| LICBAG | INTEGER | scalar | Length/index for airbag interface connectivity. | airbag/fluid | airbag_control_t |
| LRCBAG | INTEGER | scalar | Length/index for airbag real work arrays. | airbag/fluid | airbag_control_t |
| LIBAGJET | INTEGER | scalar | Length/index for airbag jet integer data. | airbag/fluid | airbag_control_t |
| LRBAGJET | INTEGER | scalar | Length/index for airbag jet real data. | airbag/fluid | airbag_control_t |
| LIBAGHOL | INTEGER | scalar | Length/index for airbag hole integer data. | airbag/fluid | airbag_control_t |
| LRBAGHOL | INTEGER | scalar | Length/index for airbag hole real data. | airbag/fluid | airbag_control_t |
| ISHFRAM | INTEGER | scalar | Shell-frame output/kinematics flag. | global solver control | simulation_control_t |
| TRIMAT | INTEGER | scalar | Triangular material/trim option flag. | global solver control | simulation_control_t |
| IUSHELL | INTEGER | scalar | User shell law/model flag. | element/material state | material_state_layout_t |
| IUSOLID | INTEGER | scalar | User solid law/model flag. | element/material state | material_state_layout_t |
| NUSHELL | INTEGER | scalar | Number of user shell variables. | element/material state | material_state_layout_t |
| NUSOLID | INTEGER | scalar | Number of user solid variables. | element/material state | material_state_layout_t |
| NVSHELL | INTEGER | scalar | Number of shell state variables. | element/material state | material_state_layout_t |
| INISHVAR | INTEGER | scalar | Initial shell-variable flag. | global solver control | simulation_control_t |
| NSPROC | INTEGER | scalar | Number of solver processes requested/used. | global solver control | simulation_control_t |
| NDSOLV | INTEGER | scalar | Solver decomposition/domain-solver count. | global solver control | simulation_control_t |
| NSBMAX | INTEGER | scalar | Maximum number of state blocks/sub-blocks. | global solver control | simulation_control_t |
| NSVMAXT | INTEGER | scalar | Maximum state-variable count. | global solver control | simulation_control_t |
| NFLOW | INTEGER | scalar | Number of flow entities/options. | airbag/fluid | airbag_control_t |
| ICONDP | INTEGER | scalar | Conduction/thermal coupling flag. | global solver control | simulation_control_t |
| NVSOLID1 | INTEGER | scalar | Solid state-variable family 1 count. | element/material state | material_state_layout_t |
| NVSOLID2 | INTEGER | scalar | Solid state-variable family 2 count. | element/material state | material_state_layout_t |
| NFASOLFR | INTEGER | scalar | Count of failure-solution fronts/free faces. | global solver control | simulation_control_t |
| KCONTACT | INTEGER | scalar | Global presence flag for contact interfaces/penalty logic. | contact/interfaces | contact_control_t |
| IORTSHEL | INTEGER | scalar | Integer flag/index for `ORTSHEL`. | global solver control | simulation_control_t |
| NORTSHEL | INTEGER | scalar | Count/flag for `ORTSHEL`. | global solver control | simulation_control_t |
| ICRACK | INTEGER | scalar | Crack/XFEM feature enabled flag. | XFEM/crack | xfem_control_t |
| IUSRSHEL | INTEGER | scalar | Integer flag/index for `USRSHEL`. | global solver control | simulation_control_t |
| IMPOSE_DR | INTEGER | scalar | Imposed displacement-rate option flag. | global solver control | simulation_control_t |
| IRIGID_MAT | INTEGER | scalar | Rigid-material option flag. | global solver control | simulation_control_t |
| ISTAMPING | INTEGER | scalar | Stamping workflow enabled flag. | global solver control | simulation_control_t |
| IINTTHICK | INTEGER | scalar | Integer flag/index for `INTTHICK`. | time integration and stability | time_control_t |
| IPLYXFEM | INTEGER | scalar | XFEM ply output/processing flag. | XFEM/crack | xfem_control_t |
| IPLYBCS | INTEGER | scalar | Ply boundary-condition processing flag. | element/material state | material_state_layout_t |
| IREAC | INTEGER | scalar | Reaction-force computation flag. | global solver control | simulation_control_t |
| IGRELEM | INTEGER | scalar | Group-element processing flag. | global solver control | simulation_control_t |
| IREST_MSELT | INTEGER | scalar | Restart handling flag for selected materials/elements. | global solver control | simulation_control_t |
| NTHREAD | INTEGER | scalar | Thread count. | parallel/runtime | parallel_sync_t |
| IALELAG | INTEGER | scalar | ALE/Lagrange coupling mode flag. | ALE | ale_control_t |
| INVOL | INTEGER | scalar | Volume-monitoring flag. | global solver control | simulation_control_t |
| IIMPLICIT | INTEGER | scalar | Implicit-solver mode flag. | global solver control | simulation_control_t |
| IRODDL0 | INTEGER | scalar | Reference odd/even DOF mode flag. | global solver control | simulation_control_t |
| NVSOLID3 | INTEGER | scalar | Solid state-variable family 3 count. | element/material state | material_state_layout_t |
| IMASADD | INTEGER | scalar | Starter count/flag for /ADMASS definitions. | global solver control | simulation_control_t |
| IDAMP_RDOF | INTEGER | scalar | Rotational-DOF damping flag. | time integration and stability | time_control_t |
| KMP_SET | INTEGER | scalar | KMP/OpenMP environment set flag. | parallel/runtime | parallel_sync_t |
| KMPSTSIZ | INTEGER | scalar | KMP/OpenMP status buffer size. | parallel/runtime | parallel_sync_t |
| ICRASH | INTEGER | scalar | Crash-analysis mode flag. | global solver control | simulation_control_t |
| STACKSIZ | INTEGER | scalar | Composite stack buffer size. | element/material state | material_state_layout_t |
| FTEMPVAR21 | INTEGER | scalar | Reserved placeholder field. | global solver control | simulation_control_t |
| NVSOLID4 | INTEGER | scalar | Solid state-variable family 4 count. | element/material state | material_state_layout_t |
| INTPLYXFEM | INTEGER | scalar | Interface/XFEM ply coupling flag. | XFEM/crack | xfem_control_t |
| UNITMAX | INTEGER | scalar | Maximum logical unit/file-unit value. | global solver control | simulation_control_t |
| NVSHELL1 | INTEGER | scalar | Shell state-variable family 1 count. | element/material state | material_state_layout_t |
| NFILSOL | INTEGER | scalar | Number of solution files / solution-file flag. | global solver control | simulation_control_t |
| IPART_STACK | INTEGER | scalar | Composite stack-part workflow enabled flag. | element/material state | material_state_layout_t |
| TEST_POIDS | INTEGER | scalar | Mass/weight test flag. | global solver control | simulation_control_t |
| IPRIVATE | INTEGER | scalar | Private-memory / private-data flag. | global solver control | simulation_control_t |
| IPERTURB | INTEGER | scalar | Perturbation-analysis flag. | global solver control | simulation_control_t |
| IPART_PCOMPP | INTEGER | scalar | Composite-part preprocessing flag. | loads, groups, and BCs | entity_counts_t |
| ITHKSHEL | INTEGER | scalar | Shell-thickness processing flag. | element/material state | material_state_layout_t |
| ISH3NFRAM | INTEGER | scalar | 3-node shell frame option flag. | global solver control | simulation_control_t |
| IUFACYLD | INTEGER | scalar | User yield-surface/failure flag. | global solver control | simulation_control_t |
| NVSHELL2 | INTEGER | scalar | Shell state-variable family 2 count. | element/material state | material_state_layout_t |
| NVSOLID5 | INTEGER | scalar | Solid state-variable family 5 count. | element/material state | material_state_layout_t |
| NUBEAM | INTEGER | scalar | Number of user beam variables. | element/material state | material_state_layout_t |
| NVBEAM | INTEGER | scalar | Number of beam state variables. | element/material state | material_state_layout_t |
| NVTRUSS | INTEGER | scalar | Number of truss state variables. | element/material state | material_state_layout_t |
| IS17OLD | INTEGER | scalar | Legacy /INTER/TYPE17 compatibility flag. | global solver control | simulation_control_t |
| NSEGQUADFR | INTEGER | scalar | Count of quad facets/segments for fracture/contact. | curves/tables/integration | registry_tables_t |
| NUMBCSN | INTEGER | scalar | Number of nodal boundary conditions. | loads, groups, and BCs | entity_counts_t |
| INTERADHESION | INTEGER | scalar | Global flag for adhesive/thermal interface treatment. | contact/interfaces | contact_control_t |
| NITSCHE | INTEGER | scalar | Global flag enabling Nitsche contact formulation. | contact/interfaces | contact_control_t |
| INISPRI | INTEGER | scalar | Initial spring-state generation flag. | element/material state | material_state_layout_t |
| INISHVAR1 | INTEGER | scalar | Additional initial state-variable flag. | global solver control | simulation_control_t |
| NVSPRI | INTEGER | scalar | Number of spring state variables. | element/material state | material_state_layout_t |
| NVSOLID6 | INTEGER | scalar | Solid state-variable family 6 count. | element/material state | material_state_layout_t |
| TH_STRAIN | INTEGER | scalar | Enable time-history strain tensor output. | animation/output | output_control_t |

### COM04 — Entity counts and model cardinalities
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/com04_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 10247 source include-sites
**Note**: Starter variant is richer because it tracks import/build-time quantities (`NMONVOL`, `NUMCNOD`, `NMERGED`, `NGR*0`, `NDEFAULT`, `FIRSTNOD_ISOGEO`, `NUMMAT_FILE`, `HM_*`, `NB_MERGE_NODE`, etc.) that are not needed in the engine runtime header.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NUMMAT | INTEGER | scalar | Number of materials. | model sizing and allocation | entity_counts_t |
| NUMNOD | INTEGER | scalar | Number of nodes. | model sizing and allocation | entity_counts_t |
| NUMSKW | INTEGER | scalar | Number of skew systems. | model sizing and allocation | entity_counts_t |
| NUMBCS | INTEGER | scalar | Number of boundary conditions. | loads, groups, and BCs | entity_counts_t |
| NANALY | INTEGER | scalar | Number of analysis definitions/options. | model sizing and allocation | entity_counts_t |
| NUMELQ | INTEGER | scalar | Total number of `ELQ` entities. | model sizing and allocation | entity_counts_t |
| NUMELS | INTEGER | scalar | Total number of `ELS` entities. | model sizing and allocation | entity_counts_t |
| NUMELC | INTEGER | scalar | Total number of `ELC` entities. | model sizing and allocation | entity_counts_t |
| NUMELT | INTEGER | scalar | Total number of `ELT` entities. | model sizing and allocation | entity_counts_t |
| NUMGEO | INTEGER | scalar | Total number of `GEO` entities. | model sizing and allocation | entity_counts_t |
| NFUNCT | INTEGER | scalar | Count/flag for `FUNCT`. | curves/tables/integration | registry_tables_t |
| NCONLD | INTEGER | scalar | Count/flag for `CONLD`. | model sizing and allocation | entity_counts_t |
| NINVEL | INTEGER | scalar | Count/flag for `INVEL`. | model sizing and allocation | entity_counts_t |
| NLASER | INTEGER | scalar | Count/flag for `LASER`. | model sizing and allocation | entity_counts_t |
| NINTER | INTEGER | scalar | Number of interfaces. | contact/interfaces | contact_control_t |
| NRWALL | INTEGER | scalar | Count/flag for `RWALL`. | model sizing and allocation | entity_counts_t |
| NRBODY | INTEGER | scalar | Number of rigid bodies. | model sizing and allocation | entity_counts_t |
| NODMAS | INTEGER | scalar | Number of nodal masses. | model sizing and allocation | entity_counts_t |
| NFXVEL | INTEGER | scalar | Number of prescribed velocities. | model sizing and allocation | entity_counts_t |
| NRIVET | INTEGER | scalar | Number of rivets. | model sizing and allocation | entity_counts_t |
| NUMELR | INTEGER | scalar | Total number of `ELR` entities. | model sizing and allocation | entity_counts_t |
| NUMELP | INTEGER | scalar | Total number of `ELP` entities. | model sizing and allocation | entity_counts_t |
| NSECT | INTEGER | scalar | Count/flag for `SECT`. | model sizing and allocation | entity_counts_t |
| NRBAG | INTEGER | scalar | Count/flag for `RBAG`. | airbag/fluid | airbag_control_t |
| NJOINT | INTEGER | scalar | Count/flag for `JOINT`. | model sizing and allocation | entity_counts_t |
| NUMELTG | INTEGER | scalar | Total number of `ELTG` entities. | model sizing and allocation | entity_counts_t |
| NSLAG | INTEGER | scalar | Count/flag for `SLAG`. | model sizing and allocation | entity_counts_t |
| NFACX | INTEGER | scalar | Count/flag for `FACX`. | model sizing and allocation | entity_counts_t |
| NUMPOR | INTEGER | scalar | Total number of `POR` entities. | model sizing and allocation | entity_counts_t |
| NACCELM | INTEGER | scalar | Count/flag for `ACCELM`. | model sizing and allocation | entity_counts_t |
| NPRETEN | INTEGER | scalar | Count/flag for `PRETEN`. | model sizing and allocation | entity_counts_t |
| NVOLU | INTEGER | scalar | Count/flag for `VOLU`. | model sizing and allocation | entity_counts_t |
| NMONVOL | INTEGER | scalar | Number of monitored volumes. | model sizing and allocation | entity_counts_t |
| NPART | INTEGER | scalar | Count/flag for `PART`. | loads, groups, and BCs | entity_counts_t |
| NSURF | INTEGER | scalar | Count/flag for `SURF`. | loads, groups, and BCs | entity_counts_t |
| NSUBS | INTEGER | scalar | Count/flag for `SUBS`. | model sizing and allocation | entity_counts_t |
| NGRAV | INTEGER | scalar | Count/flag for `GRAV`. | model sizing and allocation | entity_counts_t |
| NRBY2 | INTEGER | scalar | Legacy common-block field `NRBY2`; purpose inferred mainly from naming. | loads, groups, and BCs | entity_counts_t |
| NGRNOD | INTEGER | scalar | Count/flag for `GRNOD`. | model sizing and allocation | entity_counts_t |
| NGRBRIC | INTEGER | scalar | Count/flag for `GRBRIC`. | model sizing and allocation | entity_counts_t |
| NGRQUAD | INTEGER | scalar | Count/flag for `GRQUAD`. | curves/tables/integration | registry_tables_t |
| NGRSHEL | INTEGER | scalar | Count/flag for `GRSHEL`. | model sizing and allocation | entity_counts_t |
| NGRSH3N | INTEGER | scalar | Legacy common-block field `NGRSH3N`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRBEAM | INTEGER | scalar | Count/flag for `GRBEAM`. | element/material state | material_state_layout_t |
| NGRTRUS | INTEGER | scalar | Count/flag for `GRTRUS`. | model sizing and allocation | entity_counts_t |
| NGRSPRI | INTEGER | scalar | Count/flag for `GRSPRI`. | element/material state | material_state_layout_t |
| NLINK | INTEGER | scalar | Count/flag for `LINK`. | model sizing and allocation | entity_counts_t |
| INVSTR | INTEGER | scalar | Integer flag/index for `NVSTR`. | model sizing and allocation | entity_counts_t |
| NSLIN | INTEGER | scalar | Count/flag for `SLIN`. | model sizing and allocation | entity_counts_t |
| NUMELX | INTEGER | scalar | Total number of `ELX` entities. | model sizing and allocation | entity_counts_t |
| NCONX | INTEGER | scalar | Count/flag for `CONX`. | model sizing and allocation | entity_counts_t |
| ISUMNX | INTEGER | scalar | Integer flag/index for `SUMNX`. | model sizing and allocation | entity_counts_t |
| NANIM1D | INTEGER | scalar | Legacy common-block field `NANIM1D`; purpose inferred mainly from naming. | animation/output | output_control_t |
| NR2RLNK | INTEGER | scalar | Legacy common-block field `NR2RLNK`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NUMFRAM | INTEGER | scalar | Total number of `FRAM` entities. | model sizing and allocation | entity_counts_t |
| SCODVER | INTEGER | scalar | Legacy common-block field `SCODVER`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| SMINVER | INTEGER | scalar | Legacy common-block field `SMINVER`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| SSRCVER | INTEGER | scalar | Legacy common-block field `SSRCVER`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NUMELS10 | INTEGER | scalar | Total number of `ELS10` entities. | model sizing and allocation | entity_counts_t |
| NUMELS20 | INTEGER | scalar | Total number of `ELS20` entities. | model sizing and allocation | entity_counts_t |
| IDAMPG | INTEGER | scalar | Integer flag/index for `DAMPG`. | time integration and stability | time_control_t |
| NIBVEL | INTEGER | scalar | Count/flag for `IBVEL`. | model sizing and allocation | entity_counts_t |
| NUMELS16 | INTEGER | scalar | Total number of `ELS16` entities. | model sizing and allocation | entity_counts_t |
| NUMELS8 | INTEGER | scalar | Total number of `ELS8` entities. | model sizing and allocation | entity_counts_t |
| NACTIV | INTEGER | scalar | Count/flag for `ACTIV`. | model sizing and allocation | entity_counts_t |
| NDAMP | INTEGER | scalar | Count/flag for `DAMP`. | time integration and stability | time_control_t |
| NUMELTG6 | INTEGER | scalar | Total number of `ELTG6` entities. | model sizing and allocation | entity_counts_t |
| NUMELS8A | INTEGER | scalar | Total number of `ELS8A` entities. | model sizing and allocation | entity_counts_t |
| NRBYKIN | INTEGER | scalar | Count/flag for `RBYKIN`. | loads, groups, and BCs | entity_counts_t |
| NBCSKIN | INTEGER | scalar | Count/flag for `BCSKIN`. | loads, groups, and BCs | entity_counts_t |
| SEGINDX | INTEGER | scalar | Legacy common-block field `SEGINDX`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NEXMAD | INTEGER | scalar | Count/flag for `EXMAD`. | model sizing and allocation | entity_counts_t |
| NMADPRT | INTEGER | scalar | Count/flag for `MADPRT`. | model sizing and allocation | entity_counts_t |
| NMADSH4 | INTEGER | scalar | Legacy common-block field `NMADSH4`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NMADSH3 | INTEGER | scalar | Legacy common-block field `NMADSH3`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NMADSOL | INTEGER | scalar | Count/flag for `MADSOL`. | model sizing and allocation | entity_counts_t |
| NMADNOD | INTEGER | scalar | Count/flag for `MADNOD`. | model sizing and allocation | entity_counts_t |
| NFXBODY | INTEGER | scalar | Count/flag for `FXBODY`. | model sizing and allocation | entity_counts_t |
| NEIG | INTEGER | scalar | Count/flag for `EIG`. | model sizing and allocation | entity_counts_t |
| NINTSUB | INTEGER | scalar | Count/flag for `INTSUB`. | model sizing and allocation | entity_counts_t |
| NUMCNOD | INTEGER | scalar | Number of control/constraint nodes. | model sizing and allocation | entity_counts_t |
| NVENTTOT | INTEGER | scalar | Total number of monitored airbag vents. | airbag/fluid | airbag_control_t |
| NUMNOD0 | INTEGER | scalar | Total number of `NOD0` entities. | model sizing and allocation | entity_counts_t |
| NUMELC0 | INTEGER | scalar | Total number of `ELC0` entities. | model sizing and allocation | entity_counts_t |
| NUMELTG0 | INTEGER | scalar | Total number of `ELTG0` entities. | model sizing and allocation | entity_counts_t |
| NMERGED | INTEGER | scalar | Number of merged entities. | model sizing and allocation | entity_counts_t |
| NRBYM | INTEGER | scalar | Count/flag for `RBYM`. | loads, groups, and BCs | entity_counts_t |
| NGSLNRBYM | INTEGER | scalar | Count/flag for `GSLNRBYM`. | loads, groups, and BCs | entity_counts_t |
| NFRBYM | INTEGER | scalar | Count/flag for `FRBYM`. | loads, groups, and BCs | entity_counts_t |
| NIRBYM | INTEGER | scalar | Count/flag for `IRBYM`. | loads, groups, and BCs | entity_counts_t |
| NRBE3 | INTEGER | scalar | Legacy common-block field `NRBE3`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRNOD0 | INTEGER | scalar | Legacy common-block field `NGRNOD0`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRBRIC0 | INTEGER | scalar | Legacy common-block field `NGRBRIC0`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRQUAD0 | INTEGER | scalar | Legacy common-block field `NGRQUAD0`; purpose inferred mainly from naming. | curves/tables/integration | registry_tables_t |
| NGRSHEL0 | INTEGER | scalar | Legacy common-block field `NGRSHEL0`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRSH3N0 | INTEGER | scalar | Legacy common-block field `NGRSH3N0`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRBEAM0 | INTEGER | scalar | Legacy common-block field `NGRBEAM0`; purpose inferred mainly from naming. | element/material state | material_state_layout_t |
| NGRTRUS0 | INTEGER | scalar | Legacy common-block field `NGRTRUS0`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NGRSPRI0 | INTEGER | scalar | Legacy common-block field `NGRSPRI0`; purpose inferred mainly from naming. | element/material state | material_state_layout_t |
| NSURF0 | INTEGER | scalar | Legacy common-block field `NSURF0`; purpose inferred mainly from naming. | loads, groups, and BCs | entity_counts_t |
| NSLIN0 | INTEGER | scalar | Legacy common-block field `NSLIN0`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NPLYXFEM | INTEGER | scalar | Number of XFEM ply objects. | XFEM/crack | xfem_control_t |
| NGRPART | INTEGER | scalar | Number of part groups. | loads, groups, and BCs | entity_counts_t |
| NGRPART0 | INTEGER | scalar | Number of original part groups. | loads, groups, and BCs | entity_counts_t |
| NGPE | INTEGER | scalar | Count/flag for `GPE`. | model sizing and allocation | entity_counts_t |
| NTHPART | INTEGER | scalar | Count/flag for `THPART`. | loads, groups, and BCs | entity_counts_t |
| NTABLE | INTEGER | scalar | Count/flag for `TABLE`. | curves/tables/integration | registry_tables_t |
| NBBOX | INTEGER | scalar | Number of bounding boxes. | model sizing and allocation | entity_counts_t |
| NBBOX0 | INTEGER | scalar | Number of original bounding boxes. | model sizing and allocation | entity_counts_t |
| NRBE2 | INTEGER | scalar | Legacy common-block field `NRBE2`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NHRBE2 | INTEGER | scalar | Legacy common-block field `NHRBE2`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NSTATE | INTEGER | scalar | Number of state definitions. | animation/output | output_control_t |
| NUMNODXFE | INTEGER | scalar | Number of XFEM enriched nodes. | XFEM/crack | xfem_control_t |
| NUMELCXFE | INTEGER | scalar | Count of XFEM cohesive/crack-coupled elements. | XFEM/crack | xfem_control_t |
| NUMELTGXFE | INTEGER | scalar | Count of XFEM thick-shell crack-coupled elements. | XFEM/crack | xfem_control_t |
| NBGAUGE | INTEGER | scalar | Count/flag for `BGAUGE`. | model sizing and allocation | entity_counts_t |
| NLOAD | INTEGER | scalar | Count/flag for `LOAD`. | loads, groups, and BCs | entity_counts_t |
| DIM_FRONT_SKY | INTEGER | scalar | Skyline/front dimension for starter assembly. | model sizing and allocation | entity_counts_t |
| NTRANSF | INTEGER | scalar | Number of transformation definitions. | model sizing and allocation | entity_counts_t |
| NLOADC | INTEGER | scalar | Count/flag for `LOADC`. | loads, groups, and BCs | entity_counts_t |
| NLOADP | INTEGER | scalar | Count/flag for `LOADP`. | loads, groups, and BCs | entity_counts_t |
| SIZFIELD | INTEGER | scalar | Legacy common-block field `SIZFIELD`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| SIZLOADP | INTEGER | scalar | Legacy common-block field `SIZLOADP`; purpose inferred mainly from naming. | loads, groups, and BCs | entity_counts_t |
| NALELK | INTEGER | scalar | Number of ALE-linked entities. | ALE | ale_control_t |
| NCLUSTER | INTEGER | scalar | Count/flag for `CLUSTER`. | model sizing and allocation | entity_counts_t |
| NDEFAULT | INTEGER | scalar | Number of default definitions/cards. | model sizing and allocation | entity_counts_t |
| NUMELIG3D | INTEGER | scalar | Total number of `ELIG3D` entities. | model sizing and allocation | entity_counts_t |
| FIRSTNOD_ISOGEO | INTEGER | scalar | First isogeometric node id. | model sizing and allocation | entity_counts_t |
| ISFEM | INTEGER | scalar | Integer flag/index for `SFEM`. | model sizing and allocation | entity_counts_t |
| NLOADB | INTEGER | scalar | Number of block loads. | loads, groups, and BCs | entity_counts_t |
| SIZLOADB | INTEGER | scalar | Storage size for block loads. | loads, groups, and BCs | entity_counts_t |
| NLOADP_F | INTEGER | scalar | Legacy common-block field `NLOADP_F`; purpose inferred mainly from naming. | loads, groups, and BCs | entity_counts_t |
| NUMNOR | INTEGER | scalar | Total number of `NOR` entities. | model sizing and allocation | entity_counts_t |
| NINTER25 | INTEGER | scalar | Legacy common-block field `NINTER25`; purpose inferred mainly from naming. | contact/interfaces | contact_control_t |
| NPERTURB | INTEGER | scalar | Number of perturbation definitions. | model sizing and allocation | entity_counts_t |
| NUMSTACK | INTEGER | scalar | Number of composite stacks. | element/material state | material_state_layout_t |
| NUMPLY | INTEGER | scalar | Number of plies. | element/material state | material_state_layout_t |
| NS10E | INTEGER | scalar | Legacy common-block field `NS10E`; purpose inferred mainly from naming. | model sizing and allocation | entity_counts_t |
| NINIGRAV | INTEGER | scalar | Number of initial-gravity definitions. | model sizing and allocation | entity_counts_t |
| NINTERFRIC | INTEGER | scalar | Count/flag for `INTERFRIC`. | contact/interfaces | contact_control_t |
| NINIMAP1D | INTEGER | scalar | Number of 1D initial-mapping definitions. | model sizing and allocation | entity_counts_t |
| NFUNC2D | INTEGER | scalar | Number of 2D functions. | curves/tables/integration | registry_tables_t |
| NINIMAP2D | INTEGER | scalar | Number of 2D initial-mapping definitions. | model sizing and allocation | entity_counts_t |
| NUMFAKENODIGEO | INTEGER | scalar | Number of fake isogeometric nodes. | model sizing and allocation | entity_counts_t |
| NUMNODIGE0 | INTEGER | scalar | Original number of isogeometric nodes. | model sizing and allocation | entity_counts_t |
| NRBMERGE | INTEGER | scalar | Number of rigid-body merge operations. | model sizing and allocation | entity_counts_t |
| NUMMAT_FILE | INTEGER | scalar | Material count read from file. | model sizing and allocation | entity_counts_t |
| HM_NUMMAT | INTEGER | scalar | Material count imported from HyperMesh metadata. | model sizing and allocation | entity_counts_t |
| NUMGEO_FILE | INTEGER | scalar | Property/geometry count read from file. | model sizing and allocation | entity_counts_t |
| HM_NUMGEO | INTEGER | scalar | Property/geometry count imported from HyperMesh metadata. | model sizing and allocation | entity_counts_t |
| NUMBRICK | INTEGER | scalar | Number of brick elements. | model sizing and allocation | entity_counts_t |
| NUMTETRA4 | INTEGER | scalar | Number of tetra4 elements. | model sizing and allocation | entity_counts_t |
| NUMPENTA6 | INTEGER | scalar | Number of penta6 elements. | model sizing and allocation | entity_counts_t |
| NXTRA_NODE | INTEGER | scalar | Number of extra/generated nodes. | model sizing and allocation | entity_counts_t |
| NUMELTRIA | INTEGER | scalar | Number of triangular elements. | model sizing and allocation | entity_counts_t |
| HM_NINVEL | INTEGER | scalar | Initial-velocity count imported from HyperMesh metadata. | model sizing and allocation | entity_counts_t |
| NINVEL_FILE | INTEGER | scalar | Initial-velocity count read from file. | model sizing and allocation | entity_counts_t |
| HM_NINTER | INTEGER | scalar | Interface count imported from HyperMesh metadata. | contact/interfaces | contact_control_t |
| NFRIC_ORIENT | INTEGER | scalar | Number of friction-orientation definitions. | model sizing and allocation | entity_counts_t |
| NBCSCYC | INTEGER | scalar | Count/flag for `BCSCYC`. | loads, groups, and BCs | entity_counts_t |
| NLOADP_HYD | INTEGER | scalar | Number of hydrodynamic pressure loads. | loads, groups, and BCs | entity_counts_t |
| NINTLOADP | INTEGER | scalar | Number of interface-coupled pressure-load relations. | loads, groups, and BCs | entity_counts_t |
| NINTLOADP21 | INTEGER | scalar | Number of type-21 pressure-load couplings. | loads, groups, and BCs | entity_counts_t |
| NB_MERGE_NODE | INTEGER | scalar | Number of merged nodes. | model sizing and allocation | entity_counts_t |
| NRBODY0 | INTEGER | scalar | Original rigid-body count. | model sizing and allocation | entity_counts_t |

### COM06 — Time-step factors, damping, and output intervals
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/com06_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 860 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| DTDES | REAL(WP) | scalar | Desired time step. | time integration and stability | time_control_t |
| DTHIS | REAL(WP) | scalar | Time-history output interval. | time integration and stability | time_control_t |
| DTFAC | REAL(WP) | scalar | Global time-step scale factor. | time integration and stability | time_control_t |
| DTMIN | REAL(WP) | scalar | Minimum allowed time step. | time integration and stability | time_control_t |
| VOLMIN | REAL(WP) | scalar | Minimum element/control volume threshold. | time integration and damping | time_control_t |
| REINT | REAL(WP) | scalar | Internal energy ratio/scale. | time integration and damping | time_control_t |
| UREINT | REAL(WP) | scalar | User/internal energy reference. | time integration and damping | time_control_t |
| ECONTV | REAL(WP) | scalar | Contact energy value. | time integration and damping | time_control_t |
| EHOUR | REAL(WP) | scalar | Hourglass energy value. | time integration and damping | time_control_t |
| DTOUTP | REAL(WP) | scalar | Output period for state output. | animation/output | output_control_t |
| TOUTP | REAL(WP) | scalar | Next state output time. | animation/output | output_control_t |
| T1S | REAL(WP) | scalar | Time scale for shell/solid subcycling. | time integration and damping | time_control_t |
| DT2S | REAL(WP) | scalar | Secondary time-step scale. | time integration and stability | time_control_t |
| DT12S | REAL(WP) | scalar | Half-step shell/output increment. | time integration and stability | time_control_t |
| R2RFX1 | REAL(WP) | scalar | Rad2Rad coupling factor 1. | time integration and damping | time_control_t |
| R2RFX2 | REAL(WP) | scalar | Rad2Rad coupling factor 2. | time integration and damping | time_control_t |
| DAMPA | REAL(WP) | scalar | Rayleigh damping coefficient A. | time integration and stability | time_control_t |
| DAMPB | REAL(WP) | scalar | Rayleigh damping coefficient B. | time integration and stability | time_control_t |
| DAMPW | REAL(WP) | scalar | Damping work/weighting factor. | time integration and stability | time_control_t |
| DTHIS1 | REAL(WP) | (9) | Per-channel time-history intervals (9 slots). | time integration and stability | time_control_t |
| DWMAD | REAL(WP) | scalar | MAD damping/work threshold. | curves/tables/integration | registry_tables_t |
| T1SH | REAL(WP) | scalar | Shell-related time scale. | time integration and damping | time_control_t |
| DTSTAT | REAL(WP) | scalar | Status print period. | animation/output | output_control_t |
| TSTAT | REAL(WP) | scalar | Next status print time. | animation/output | output_control_t |
| DTABF | REAL(WP) | (10) | ABF output periods (10 slots). | animation/output | output_control_t |
| DTABFWR | REAL(WP) | (10) | ABF write periods (10 slots). | animation/output | output_control_t |
| ECONTD | REAL(WP) | scalar | Contact energy increment. | time integration and damping | time_control_t |
| ECONT_CUMU | REAL(WP) | scalar | Cumulative contact energy. | time integration and damping | time_control_t |
| DTDYNAIN | REAL(WP) | scalar | DYNAIN output interval. | animation/output | output_control_t |
| TDYNAIN | REAL(WP) | scalar | Next DYNAIN output time. | animation/output | output_control_t |

### COM08 — Current time state and scheduling values
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/com08_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 3964 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| TT | REAL(WP) | scalar | Current physical time. | time integration and stability | time_control_t |
| DT1 | REAL(WP) | scalar | Current stable time step. | time integration and stability | time_control_t |
| DT2 | REAL(WP) | scalar | Next/proposed time step. | time integration and stability | time_control_t |
| DT12 | REAL(WP) | scalar | Half-step or predictor-corrector time increment. | time integration and stability | time_control_t |
| DT2OLD | REAL(WP) | scalar | Previous secondary time step. | time integration and stability | time_control_t |
| TSTOP | REAL(WP) | scalar | Final stop time. | time integration and stability | time_control_t |
| THIS | REAL(WP) | scalar | Current time-history output time. | time integration and scheduling | time_control_t |
| THIS1 | REAL(WP) | (9) | Per-channel time-history output times (9 slots). | time integration and scheduling | time_control_t |
| TABFIS | REAL(WP) | (10) | ABF/FIS scheduling table (10 slots). | animation/output | output_control_t |
| TABFWR | REAL(WP) | (10) | ABF write scheduling table (10 slots). | animation/output | output_control_t |

### COM09 — Output-cycle flags and last-write markers
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/com09_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 152 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IANIM | INTEGER | scalar | Animation output enabled flag. | animation/output | output_control_t |
| IOUTP | INTEGER | scalar | State-output enabled flag. | animation/output | output_control_t |
| NHIN2 | INTEGER | scalar | HIN/H3D-style history/input option flag. | animation/output scheduling | output_control_t |
| IDELI7 | INTEGER | scalar | Deletion flag for interface type 7. | animation/output scheduling | output_control_t |
| IBAGSURF | INTEGER | scalar | Airbag surface output flag. | airbag/fluid | airbag_control_t |
| ISTATF | INTEGER | scalar | Status file/output flag. | animation/output | output_control_t |
| IH3D | INTEGER | scalar | H3D output enabled flag. | animation/output | output_control_t |

### COM_ENGCARDS — Starter /RUN/ENGINE card counts
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/com_engcards_c.inc`
**Present in**: starter only
**Usage count**: 3 (observed) source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NGINE | INTEGER | scalar | Number of /RUN/ENGINE blocks. | starter engine-card preprocessing | engcards_t |
| NRUN_ENG | INTEGER | scalar | Number of run options in engine cards. | starter engine-card preprocessing | engcards_t |
| NANIM_ENG | INTEGER | scalar | Number of engine animation cards. | starter engine-card preprocessing | engcards_t |
| NTFILE_ENG | INTEGER | scalar | Number of engine time-file cards. | starter engine-card preprocessing | engcards_t |
| NSTOP_ENG | INTEGER | scalar | Number of engine stop cards. | starter engine-card preprocessing | engcards_t |
| NRFILE_ENG | INTEGER | scalar | Number of engine restart-file cards. | starter engine-card preprocessing | engcards_t |
| NDT_ENG | INTEGER | scalar | Number of engine time-step cards. | starter engine-card preprocessing | engcards_t |
| NVERS_ENG | INTEGER | scalar | Engine card version indicator. | starter engine-card preprocessing | engcards_t |

### XFEM3I — XFEM starter sizing and crack topology counts
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/com_xfem1.inc`
**Present in**: both (engine content differs)
**Usage count**: 667 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NUMELCRK | INTEGER | scalar | Number of crack/XFEM elements. | XFEM/crack | xfem_control_t |
| ICRACK3D | INTEGER | scalar | 3D crack processing flag. | XFEM/crack | xfem_control_t |
| NXEL | INTEGER | scalar | Number of enriched XFEM elements. | XFEM/crack | xfem_control_t |
| NINICRACK | INTEGER | scalar | Number of initial cracks. | XFEM/crack | xfem_control_t |
| NCRKXFE | INTEGER | scalar | Number of XFEM crack entities. | XFEM/crack | xfem_control_t |
| ECRKXFE | INTEGER | scalar | Count/size of XFEM crack-edge data. | XFEM/crack | xfem_control_t |
| ECRKXFEC | INTEGER | scalar | Count/size of cohesive crack-edge data. | XFEM/crack | xfem_control_t |
| ECRKXFETG | INTEGER | scalar | Count/size of thick-shell crack-edge data. | XFEM/crack | xfem_control_t |
| NLEVMAX | INTEGER | scalar | Maximum XFEM level-set level. | XFEM/crack | xfem_control_t |
| NUMEDGES | INTEGER | scalar | Number of crack edges. | XFEM/crack | xfem_control_t |
| NXLAYMAX | INTEGER | scalar | Maximum XFEM layer count. | XFEM/crack | xfem_control_t |
| IENRNOD | INTEGER | scalar | Enriched-node flag/count. | XFEM/crack | xfem_control_t |
| LCNECRKXFEM | INTEGER | scalar | Length/index for starter crack/XFEM connectivity. | XFEM/crack | xfem_control_t |

### CMDLINE — Command-line option flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/commandline.inc`
**Present in**: both (engine content differs)
**Usage count**: 12 (observed) source include-sites
**Note**: Starter adds CPU, timer, user-library, and HSTP read/write flags beyond the engine subset.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| GOT_NCPU | INTEGER | scalar | Command-line flag: CPU count specified. | startup and CLI parsing | commandline_opts_t |
| NCPU | INTEGER | scalar | Requested CPU count. | startup and CLI parsing | commandline_opts_t |
| GOT_NTH | INTEGER | scalar | Command-line flag: thread count specified. | startup and CLI parsing | commandline_opts_t |
| NTH | INTEGER | scalar | Requested thread count. | startup and CLI parsing | commandline_opts_t |
| GOT_TIMER | INTEGER | scalar | Command-line flag: timer option specified. | startup and CLI parsing | commandline_opts_t |
| GOT_DUSERL | INTEGER | scalar | Command-line flag: user library specified. | startup and CLI parsing | commandline_opts_t |
| GOT_MEM_MAP | INTEGER | scalar | Command-line flag: memory-map option specified. | startup and CLI parsing | commandline_opts_t |
| GOT_INSPIRE | INTEGER | scalar | Command-line flag: Inspire mode specified. | startup and CLI parsing | commandline_opts_t |
| GOT_INSPIRE_ALM | INTEGER | scalar | Command-line flag: Inspire-ALM mode specified. | startup and CLI parsing | commandline_opts_t |
| GOT_HSTP_READ | INTEGER | scalar | Command-line flag: HSTP read requested. | startup and CLI parsing | commandline_opts_t |
| GOT_HSTP_WRITE | INTEGER | scalar | Command-line flag: HSTP write requested. | startup and CLI parsing | commandline_opts_t |

### SCR01 — Task-private engine task id / starter memory-buffer lengths
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr01_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 42 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NB1 | INTEGER | scalar | Memory buffer length counter 1. | thread-private runtime scratch | task_private_t |
| NB2 | INTEGER | scalar | Memory buffer length counter 2. | thread-private runtime scratch | task_private_t |
| NB3 | INTEGER | scalar | Memory buffer length counter 3. | thread-private runtime scratch | task_private_t |
| NB4 | INTEGER | scalar | Memory buffer length counter 4. | thread-private runtime scratch | task_private_t |
| NB5 | INTEGER | scalar | Memory buffer length counter 5. | thread-private runtime scratch | task_private_t |
| NB6 | INTEGER | scalar | Memory buffer length counter 6. | thread-private runtime scratch | task_private_t |
| NB7 | INTEGER | scalar | Memory buffer length counter 7. | thread-private runtime scratch | task_private_t |
| NB8 | INTEGER | scalar | Memory buffer length counter 8. | thread-private runtime scratch | task_private_t |
| NB9 | INTEGER | scalar | Memory buffer length counter 9. | thread-private runtime scratch | task_private_t |
| NB10 | INTEGER | scalar | Memory buffer length counter 10. | thread-private runtime scratch | task_private_t |
| NB11 | INTEGER | scalar | Memory buffer length counter 11. | thread-private runtime scratch | task_private_t |
| NB12 | INTEGER | scalar | Memory buffer length counter 12. | thread-private runtime scratch | task_private_t |
| NB13 | INTEGER | scalar | Memory buffer length counter 13. | thread-private runtime scratch | task_private_t |
| NB14 | INTEGER | scalar | Memory buffer length counter 14. | thread-private runtime scratch | task_private_t |
| NB15 | INTEGER | scalar | Memory buffer length counter 15. | thread-private runtime scratch | task_private_t |
| NB16 | INTEGER | scalar | Memory buffer length counter 16. | thread-private runtime scratch | task_private_t |
| NB17 | INTEGER | scalar | Memory buffer length counter 17. | thread-private runtime scratch | task_private_t |
| NB18 | INTEGER | scalar | Memory buffer length counter 18. | thread-private runtime scratch | task_private_t |
| NB19 | INTEGER | scalar | Memory buffer length counter 19. | thread-private runtime scratch | task_private_t |
| NB20 | INTEGER | scalar | Memory buffer length counter 20. | thread-private runtime scratch | task_private_t |

### SCR03 — Versioning, preprocessing, and ALE/link metadata
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr03_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 958 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IPRI | INTEGER | scalar | Starter print level. | preprocessing and versioning | simulation_control_t |
| INVERS | INTEGER | scalar | Version id. | preprocessing and versioning | simulation_control_t |
| IFORM8 | INTEGER | scalar | 8-byte format flag. | preprocessing and versioning | simulation_control_t |
| IMESH | INTEGER | scalar | Mesh import flag. | preprocessing and versioning | simulation_control_t |
| NPRELD | INTEGER | scalar | Preload count. | preprocessing and versioning | simulation_control_t |
| ISNOD | INTEGER | scalar | Selected node flag/count. | preprocessing and versioning | simulation_control_t |
| IREFSTA | INTEGER | scalar | Starter reference-state flag. | preprocessing and versioning | simulation_control_t |
| NXREF | INTEGER | scalar | Reference xref count. | preprocessing and versioning | simulation_control_t |
| NEREF | INTEGER | scalar | Reference entity count. | preprocessing and versioning | simulation_control_t |
| IKREM | INTEGER | scalar | Keyword-remove flag. | preprocessing and versioning | simulation_control_t |
| CODVERS | INTEGER | scalar | Code version number. | preprocessing and versioning | simulation_control_t |
| IGRAX | INTEGER | scalar | Graphics axis X flag. | preprocessing and versioning | simulation_control_t |
| IGRAY | INTEGER | scalar | Graphics axis Y flag. | preprocessing and versioning | simulation_control_t |
| INVERS_SRC | INTEGER | scalar | Source-version id. | preprocessing and versioning | simulation_control_t |
| IGRAZ | INTEGER | scalar | Graphics axis Z flag. | preprocessing and versioning | simulation_control_t |
| ICODRUN | INTEGER | scalar | Run code identifier. | preprocessing and versioning | simulation_control_t |
| IMINVER | INTEGER | scalar | Minimum compatible version. | preprocessing and versioning | simulation_control_t |
| ISRCVER | INTEGER | scalar | Source version id. | preprocessing and versioning | simulation_control_t |
| PCODVER | INTEGER | scalar | Printed/packed code version. | preprocessing and versioning | simulation_control_t |
| PMINVER | INTEGER | scalar | Printed/packed minimum version. | preprocessing and versioning | simulation_control_t |
| PSRCVER | INTEGER | scalar | Printed/packed source version. | preprocessing and versioning | simulation_control_t |
| PINVERS | INTEGER | scalar | Printed/packed input version. | preprocessing and versioning | simulation_control_t |
| PIRUN | INTEGER | scalar | Printed/packed run id. | preprocessing and versioning | simulation_control_t |
| PCODRUN | INTEGER | scalar | Legacy common-block field `PCODRUN`; purpose inferred mainly from naming. | preprocessing and versioning | simulation_control_t |
| NITRS | INTEGER | scalar | Iteration count. | preprocessing and versioning | simulation_control_t |
| IISROT | INTEGER | scalar | Rotation-system flag. | preprocessing and versioning | simulation_control_t |
| IFIX | INTEGER | scalar | Fixed-format import flag. | preprocessing and versioning | simulation_control_t |
| INVERS_INIT | INTEGER | scalar | Initial version id. | preprocessing and versioning | simulation_control_t |
| IFIX_INIT | INTEGER | scalar | Initial fixed-format flag. | preprocessing and versioning | simulation_control_t |
| PDEL | INTEGER | scalar | Deletion/patch level. | preprocessing and versioning | simulation_control_t |

### SCR04 — Thread-private relation counters / starter scratch unit
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr04_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 17 source include-sites
**Note**: In the starter this block contains only `IUNIT` (a scratch logical unit number) and does **not** carry an `!$OMP THREADPRIVATE` directive — it is a regular COMMON in the starter build. The engine version of this block is thread-private; the starter version is not.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
| IUNIT | INTEGER | scalar | Scratch logical unit number. | thread-private scratch | task_private_t |

### SCR05 — Format, architecture, and output-control switches
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr05_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 927 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ICRAY | INTEGER | scalar | CRAY/legacy binary format flag. | I/O formats and output configuration | output_control_t |
| IRFORM | INTEGER | scalar | Restart file format flag. | I/O formats and output configuration | output_control_t |
| ITFORM | INTEGER | scalar | Time-history/output file format flag. | I/O formats and output configuration | output_control_t |
| TH_VERS | INTEGER | scalar | Time-history format version. | animation/output | output_control_t |
| IRFE0 | INTEGER | scalar | Starter RFE option flag. | I/O formats and output configuration | output_control_t |
| IMOT | INTEGER | scalar | Motion import flag. | I/O formats and output configuration | output_control_t |
| IOUTPUT | INTEGER | scalar | Starter output generation flag. | animation/output | output_control_t |
| IVECTOR | INTEGER | scalar | Vectorized/packed output flag. | I/O formats and output configuration | output_control_t |
| IMPON | INTEGER | scalar | Imposed-motion option flag. | I/O formats and output configuration | output_control_t |
| IRESP | INTEGER | scalar | Response/output request flag. | I/O formats and output configuration | output_control_t |
| IARCH | INTEGER | scalar | Architecture selection flag. | I/O formats and output configuration | output_control_t |
| IBUILTIN | INTEGER | scalar | Built-in reader/writer flag. | I/O formats and output configuration | output_control_t |
| ARCHINFO | INTEGER | (MAXARCH,2) | Architecture metadata table. | I/O formats and output configuration | output_control_t |
| ARCHN | CHARACTER*40 | (MAXARCH) | Architecture entry count. | I/O formats and output configuration | output_control_t |

### SCR06R — Real-valued damping/energy scratch scalars
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr06_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 376 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| HVISC | REAL(WP) | scalar | Viscous damping/energy scale. | reporting and damping | simulation_control_t |
| HELAS | REAL(WP) | scalar | Elastic energy scale. | reporting and damping | simulation_control_t |
| REEL | REAL(WP) | scalar | Real/elastic scaling factor. | reporting and damping | simulation_control_t |
| BMUL0 | REAL(WP) | scalar | Bulk modulus multiplier/reference. | reporting and damping | simulation_control_t |

### SCR08 — Element corner coordinate scratch
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr08_c.inc`
**Present in**: starter only
**Usage count**: 45 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| XC | REAL(WP) | (8) | Element corner X coordinates. | element geometry | element_work_t |
| YC | REAL(WP) | (8) | Element corner Y coordinates. | element geometry | element_work_t |
| ZC | REAL(WP) | (8) | Element corner Z coordinates. | element geometry | element_work_t |

### SCRALE1 — ALE boundary-condition count
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr10_c.inc`
**Present in**: starter only
**Usage count**: 4 (observed) source include-sites
**Note**: `NALEBCS` is counted from `/ALE/BCS` cards in starter control reading and reported in starter summary output.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NALEBCS | INTEGER | scalar | Number of ALE boundary conditions. | ALE | ale_control_t |

### SCR11 — Energy/momentum balance or starter shell-mass scratch
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr11_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 270 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| YL | REAL(WP) | scalar | Local Y length/coordinate scratch. | energy accounting | energy_balance_t |
| ZL | REAL(WP) | scalar | Local Z length/coordinate scratch. | energy accounting | energy_balance_t |
| YD | REAL(WP) | scalar | Local Y derivative/delta scratch. | energy accounting | energy_balance_t |
| ZD | REAL(WP) | scalar | Local Z derivative/delta scratch. | energy accounting | energy_balance_t |
| DTO0 | REAL(WP) | scalar | Reference time increment. | time integration and stability | time_control_t |
| VDTO | REAL(WP) | scalar | Velocity-based time increment. | time integration and stability | time_control_t |
| DTO | REAL(WP) | scalar | Current output/time increment scratch. | time integration and stability | time_control_t |

### SCR12 — Reaction/export scratch flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr12_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 206 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IEXPM | INTEGER | scalar | Export/decomposition mode flag. | reactions / decomposition | simulation_control_t |
| DECTYP | INTEGER | scalar | Decomposition type. | reactions / decomposition | simulation_control_t |
| NSINT | INTEGER | scalar | Number of split/interfaces for decomposition. | reactions / decomposition | simulation_control_t |
| DECANI | INTEGER | scalar | Decomposed animation flag. | animation/output | output_control_t |
| DECMOT | INTEGER | scalar | Decomposed motion flag. | reactions / decomposition | simulation_control_t |
| DECNEQ | INTEGER | scalar | Decomposed equation flag. | reactions / decomposition | simulation_control_t |
| I7STIFS | INTEGER | scalar | Interface-7 stiffness export flag. | reactions / decomposition | simulation_control_t |
| EDGE_FILTERING | INTEGER | scalar | Edge filtering enabled flag. | reactions / decomposition | simulation_control_t |
| DDNOD_SMS | INTEGER | scalar | Domain-decomposition node/sms flag. | reactions / decomposition | simulation_control_t |

### SCR14 — Animation variable selection lists and flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr14_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 1180 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NV_ANI | INTEGER | scalar | Count of entries in the V_ANI request list. | animation output | output_control_t |
| ANIM_V | INTEGER | (MX_ANI) | Requested animation selector(s) for `V`. | animation output | output_control_t |
| NT_ANI | INTEGER | scalar | Count of entries in the T_ANI request list. | animation output | output_control_t |
| ANIM_T | INTEGER | (MX_ANI) | Requested animation selector(s) for `T`. | animation output | output_control_t |
| NE_ANI | INTEGER | scalar | Count of entries in the E_ANI request list. | animation output | output_control_t |
| ANIM_E | INTEGER | (MX_ANI) | Requested animation selector(s) for `E`. | animation output | output_control_t |
| NN_ANI | INTEGER | scalar | Count of entries in the N_ANI request list. | animation output | output_control_t |
| ANIM_N | INTEGER | (MX_ANI) | Requested animation selector(s) for `N`. | animation output | output_control_t |
| NCT_ANI | INTEGER | scalar | Count of entries in the CT_ANI request list. | animation output | output_control_t |
| ANIM_CT | INTEGER | (MX_ANI) | Requested animation selector(s) for `CT`. | animation output | output_control_t |
| NCE_ANI | INTEGER | scalar | Count of entries in the CE_ANI request list. | animation output | output_control_t |
| ANIM_CE | INTEGER | (MX_ANI) | Requested animation selector(s) for `CE`. | animation output | output_control_t |
| NST_ANI | INTEGER | scalar | Count of entries in the ST_ANI request list. | animation output | output_control_t |
| ANIM_ST | INTEGER | (MX_ANI) | Requested animation selector(s) for `ST`. | animation output | output_control_t |
| NSE_ANI | INTEGER | scalar | Count of entries in the SE_ANI request list. | animation output | output_control_t |
| ANIM_SE | INTEGER | (MX_ANI) | Requested animation selector(s) for `SE`. | animation output | output_control_t |
| NFT_ANI | INTEGER | scalar | Count of entries in the FT_ANI request list. | animation output | output_control_t |
| ANIM_FT | INTEGER | (MX_ANI) | Requested animation selector(s) for `FT`. | animation output | output_control_t |
| NFE_ANI | INTEGER | scalar | Count of entries in the FE_ANI request list. | animation output | output_control_t |
| ANIM_FE | INTEGER | (MX_ANI) | Requested animation selector(s) for `FE`. | animation output | output_control_t |
| ANIM_M | INTEGER | scalar | Requested animation selector(s) for `M`. | animation output | output_control_t |
| ANIM_K | INTEGER | scalar | Requested animation selector(s) for `K`. | animation output | output_control_t |
| ANIM_U | INTEGER | scalar | Requested animation selector(s) for `U`. | animation output | output_control_t |
| ANIM_MAT | INTEGER | scalar | Requested animation selector(s) for `MAT`. | animation output | output_control_t |
| IEPSDOT | INTEGER | scalar | Integer flag/index for `EPSDOT`. | animation output | output_control_t |
| FMT_ANI | INTEGER | scalar | Legacy common-block field `FMT_ANI`; purpose inferred mainly from naming. | animation output | output_control_t |
| ANIM_VERS | INTEGER | scalar | Requested animation selector(s) for `VERS`. | animation output | output_control_t |
| IZIP | INTEGER | scalar | Integer flag/index for `ZIP`. | animation output | output_control_t |
| IFVANI | INTEGER | scalar | Integer flag/index for `FVANI`. | animation output | output_control_t |
| IFAILA | INTEGER | scalar | Integer flag/index for `FAILA`. | animation output | output_control_t |
| IAD_GPS | INTEGER | scalar | Integer flag/index for `AD_GPS`. | animation output | output_control_t |
| IZIPSTRS | INTEGER | scalar | Integer flag/index for `ZIPSTRS`. | animation output | output_control_t |
| ANIM_PLY | INTEGER | scalar | Requested animation selector(s) for `PLY`. | animation output | output_control_t |
| ANIM_CRK | INTEGER | scalar | Requested animation selector(s) for `CRK`. | animation output | output_control_t |
| TITLETAB | CHARACTER*80 | (MX_ANI) | Time/state value for `ITLETAB`. | animation output | output_control_t |
| NTITLETAB | INTEGER | (MX_ANI) | Count/flag for `TITLETAB`. | animation output | output_control_t |
| NLTITLE | INTEGER | scalar | Count/flag for `LTITLE`. | animation output | output_control_t |
| ISTRESALL | INTEGER | scalar | Integer flag/index for `STRESALL`. | animation output | output_control_t |
| ISTRAIALL | INTEGER | scalar | Integer flag/index for `STRAIALL`. | animation output | output_control_t |
| IEPSDOALL | INTEGER | scalar | Integer flag/index for `EPSDOALL`. | animation output | output_control_t |
| IEPSPALL | INTEGER | scalar | Integer flag/index for `EPSPALL`. | animation output | output_control_t |
| IORTHDALL | INTEGER | scalar | Integer flag/index for `ORTHDALL`. | animation output | output_control_t |
| IEPSPFULL | INTEGER | scalar | Integer flag/index for `EPSPFULL`. | animation output | output_control_t |
| ISTRESFULL | INTEGER | scalar | Integer flag/index for `STRESFULL`. | animation output | output_control_t |
| IPLYALL | INTEGER | scalar | Integer flag/index for `PLYALL`. | animation output | output_control_t |
| ISTRESALL_PLY | INTEGER | scalar | Integer flag/index for `STRESALL_PLY`. | animation output | output_control_t |
| ISTRESALL_PLY_IPT | INTEGER | scalar | Integer flag/index for `STRESALL_PLY_IPT`. | animation output | output_control_t |
| IPHIALL_PLY | INTEGER | scalar | Integer flag/index for `PHIALL_PLY`. | animation output | output_control_t |
| IEPSPALL_PLY | INTEGER | scalar | Integer flag/index for `EPSPALL_PLY`. | animation output | output_control_t |
| ISTRAINALL_PLY | INTEGER | scalar | Integer flag/index for `STRAINALL_PLY`. | animation output | output_control_t |
| IDAMAFULL | INTEGER | scalar | Integer flag/index for `DAMAFULL`. | animation output | output_control_t |
| ISTRAINFULL | INTEGER | scalar | Integer flag/index for `STRAINFULL`. | animation output | output_control_t |
| IEPSDOFULL | INTEGER | scalar | Integer flag/index for `EPSDOFULL`. | animation output | output_control_t |
| IPHIALL | INTEGER | scalar | Integer flag/index for `PHIALL`. | animation output | output_control_t |
| IWPLAFULL | INTEGER | scalar | Integer flag/index for `WPLAFULL`. | animation output | output_control_t |
| IWPLAALL | INTEGER | scalar | Integer flag/index for `WPLAALL`. | animation output | output_control_t |
| IDAMAALL | INTEGER | scalar | Integer flag/index for `DAMAALL`. | animation output | output_control_t |
| INXTFALL | INTEGER | scalar | Integer flag/index for `NXTFALL`. | animation output | output_control_t |
| SIGH1ALL | INTEGER | scalar | Legacy common-block field `SIGH1ALL`; purpose inferred mainly from naming. | animation output | output_control_t |
| SIGH2ALL | INTEGER | scalar | Legacy common-block field `SIGH2ALL`; purpose inferred mainly from naming. | animation output | output_control_t |

### SCR15 — Root job/file name
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr15_c.inc`
**Present in**: starter only
**Usage count**: 29 (observed) source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ROOTNAM | CHARACTER*80 | scalar | Root job/file name. | file naming | file_io_ctrl_t |

### SCR15I — Root job/file name length
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr15_c.inc`
**Present in**: starter only
**Usage count**: 29 (observed) source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ROOTLEN | INTEGER | scalar | Effective length of ROOTNAM. | file naming | file_io_ctrl_t |

### SCR16R — State-output time base
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr16_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 956 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| DTOUTP0 | REAL(WP) | scalar | Reference/base output interval. | state output scheduling | output_control_t |
| TOUTP0 | REAL(WP) | scalar | Reference/base next-output time. | state output scheduling | output_control_t |

### SCR16 — State-output selection lists
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr16_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 956 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NV_OUTP | INTEGER | scalar | Count of entries in the V_OUTP request list. | state output selection | output_control_t |
| OUTP_V | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `V`. | state output selection | output_control_t |
| NN_OUTP | INTEGER | scalar | Count of entries in the N_OUTP request list. | state output selection | output_control_t |
| OUTP_N | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `N`. | state output selection | output_control_t |
| NSS_OUTP | INTEGER | scalar | Count of entries in the SS_OUTP request list. | state output selection | output_control_t |
| OUTP_SS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `SS`. | state output selection | output_control_t |
| NCS_OUTP | INTEGER | scalar | Count of entries in the CS_OUTP request list. | state output selection | output_control_t |
| OUTP_CS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `CS`. | state output selection | output_control_t |
| NTS_OUTP | INTEGER | scalar | Count of entries in the TS_OUTP request list. | state output selection | output_control_t |
| OUTP_TS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `TS`. | state output selection | output_control_t |
| NPS_OUTP | INTEGER | scalar | Count of entries in the PS_OUTP request list. | state output selection | output_control_t |
| OUTP_PS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `PS`. | state output selection | output_control_t |
| NRS_OUTP | INTEGER | scalar | Count of entries in the RS_OUTP request list. | state output selection | output_control_t |
| OUTP_RS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `RS`. | state output selection | output_control_t |
| NST_OUTP | INTEGER | scalar | Count of entries in the ST_OUTP request list. | state output selection | output_control_t |
| OUTP_ST | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `ST`. | state output selection | output_control_t |
| NCT_OUTP | INTEGER | scalar | Count of entries in the CT_OUTP request list. | state output selection | output_control_t |
| OUTP_CT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `CT`. | state output selection | output_control_t |
| NTT_OUTP | INTEGER | scalar | Count of entries in the TT_OUTP request list. | state output selection | output_control_t |
| OUTP_TT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `TT`. | state output selection | output_control_t |
| NPT_OUTP | INTEGER | scalar | Count of entries in the PT_OUTP request list. | state output selection | output_control_t |
| OUTP_PT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `PT`. | state output selection | output_control_t |
| NRT_OUTP | INTEGER | scalar | Count of entries in the RT_OUTP request list. | state output selection | output_control_t |
| OUTP_RT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `RT`. | state output selection | output_control_t |
| IOUTP_FMT | INTEGER | scalar | Integer flag/index for `OUTP_FMT`. | state output selection | output_control_t |
| OUTYY_FMT | INTEGER | scalar | Legacy common-block field `OUTYY_FMT`; purpose inferred mainly from naming. | state output selection | output_control_t |
| NSPS_OUTP | INTEGER | scalar | Count of entries in the SPS_OUTP request list. | state output selection | output_control_t |
| OUTP_SPS | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `SPS`. | state output selection | output_control_t |
| NSPT_OUTP | INTEGER | scalar | Count of entries in the SPT_OUTP request list. | state output selection | output_control_t |
| OUTP_SPT | INTEGER | (MX_OUTP) | Requested state-output selector(s) for `SPT`. | state output selection | output_control_t |
| IROOTYY | INTEGER | scalar | YY/root output flag. | state output selection | output_control_t |
| IDROT | INTEGER | scalar | Rotational output flag. | state output selection | output_control_t |
| MOUTPT | INTEGER | scalar | Output table length. | state output selection | output_control_t |
| MOUTP | INTEGER | (MX_OUTP2) | Packed output selector table. | state output selection | output_control_t |
| IROOTYY_R | INTEGER | scalar | Restart YY/root output flag. | state output selection | output_control_t |
| S0FILE | INTEGER | scalar | Starter S0 file flag/unit. | state output selection | output_control_t |

### SCR16_STATR — Status-print time base
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr16_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 956 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| DTSTAT0 | REAL(WP) | scalar | Reference/base status interval. | status output scheduling | output_control_t |
| TSTAT0 | REAL(WP) | scalar | Reference/base next-status time. | status output scheduling | output_control_t |
| DTABF0 | REAL(WP) | (10) | Reference/base ABF intervals (10 slots). | status output scheduling | output_control_t |
| DTABFWR0 | REAL(WP) | (10) | Reference/base ABF write intervals (10 slots). | status output scheduling | output_control_t |

### SCR16_STATI — Status-print entity selections
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr16_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 956 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| MX_STAT0 | INTEGER | scalar | Requested number of status channels. | status output selection | output_control_t |
| NSTATPRT | INTEGER | scalar | Number of printed status entries. | status output selection | output_control_t |
| NC_STAT | INTEGER | scalar | Number of status curves/sets. | status output selection | output_control_t |
| STAT_C | INTEGER | (MX_STAT) | Legacy common-block field `STAT_C`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELC | INTEGER | scalar | Legacy common-block field `STAT_NUMELC`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELTG | INTEGER | scalar | Legacy common-block field `STAT_NUMELTG`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELC_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELC_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELTG_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELTG_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_S | INTEGER | (MX_STAT) | Legacy common-block field `STAT_S`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELS | INTEGER | scalar | Legacy common-block field `STAT_NUMELS`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELT | INTEGER | scalar | Legacy common-block field `STAT_NUMELT`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELS_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELS_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELT_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELT_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_N | INTEGER | (MX_STAT) | Legacy common-block field `STAT_N`; purpose inferred mainly from naming. | status output selection | output_control_t |
| MSTATT | INTEGER | scalar | Status table length. | status output selection | output_control_t |
| MSTAT | INTEGER | (MX_STAT2) | Packed map/buffer for `STAT`. | status output selection | output_control_t |
| STAT_NUMELR | INTEGER | scalar | Legacy common-block field `STAT_NUMELR`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELR_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELR_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_R | INTEGER | (MX_STAT) | Legacy common-block field `STAT_R`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELP | INTEGER | scalar | Legacy common-block field `STAT_NUMELP`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_NUMELP_G | INTEGER | scalar | Legacy common-block field `STAT_NUMELP_G`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_P | INTEGER | (MX_STAT) | Legacy common-block field `STAT_P`; purpose inferred mainly from naming. | status output selection | output_control_t |
| STAT_T | INTEGER | (MX_STAT) | Legacy common-block field `STAT_T`; purpose inferred mainly from naming. | status output selection | output_control_t |
| NSTATALL | INTEGER | scalar | Print all status variables flag/count. | status output selection | output_control_t |

### SCR17ID — Starter id-buffer lengths
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr17_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 3180 source include-sites
**Note**: Starter keeps only the layout/length fields needed to build id tables; deletion counters live in the engine-only `SCR17`.

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| LNOPT1 | INTEGER | scalar | Length/index of option-1 table. | id-table sizing | deletion_control_t |
| LTITR | INTEGER | scalar | Length/index of title table. | id-table sizing | deletion_control_t |
| LILSET1 | INTEGER | scalar | Length/index of level-set table 1. | id-table sizing | deletion_control_t |
| LISLIN1 | INTEGER | scalar | Length/index of line-set table 1. | id-table sizing | deletion_control_t |
| LISUB1 | INTEGER | scalar | Length/index of substructure table 1. | id-table sizing | deletion_control_t |
| LIBOX1 | INTEGER | scalar | Length/index of box table 1. | id-table sizing | deletion_control_t |
| LISURF1 | INTEGER | scalar | Length/index of surface table 1. | id-table sizing | deletion_control_t |
| LIPART1 | INTEGER | scalar | Length/index of part table 1. | id-table sizing | deletion_control_t |
| LIGRN1 | INTEGER | scalar | Length/index of group-node table 1. | id-table sizing | deletion_control_t |
| IDT1TET10 | INTEGER | scalar | Time-step/deletion control for tet10 family. | id-table sizing | deletion_control_t |
| IDT1SOL | INTEGER | scalar | Time-step/deletion control for solid family. | id-table sizing | deletion_control_t |
| IDT1SH | INTEGER | scalar | Time-step/deletion control for shell family. | id-table sizing | deletion_control_t |
| IDTS6 | INTEGER | scalar | Time-step/deletion control for 6-node shell family. | id-table sizing | deletion_control_t |
| IDTTSH | INTEGER | scalar | Time-step/deletion control for thick-shell family. | id-table sizing | deletion_control_t |

### SCR18R — Per-group timestep controls and thresholds
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr18_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 1087 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| DTFAC1 | REAL(WP) | (102) | Per-group time-step scale factors (102 slots). | time-step control | timestep_dt_t |
| DTMIN1 | REAL(WP) | (102) | Per-group minimum time steps (102 slots). | time-step control | timestep_dt_t |
| MIN_ASPECT | REAL(WP) | scalar | Minimum allowed aspect ratio. | time-step control | timestep_dt_t |
| MIN_DEFV | REAL(WP) | scalar | Minimum deformation volume. | time-step control | timestep_dt_t |
| DTIN | REAL(WP) | scalar | Input/initial time step. | time-step control | timestep_dt_t |
| DTMX | REAL(WP) | scalar | Maximum allowed time step. | time-step control | timestep_dt_t |
| MASS0_START | REAL(WP) | scalar | Initial mass at start of added-mass monitoring. | time-step control | timestep_dt_t |

### SCR18 — Per-group timestep mode flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr18_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 1087 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IDTMIN | INTEGER | (102) | Per-group time-step enforcement modes (102 slots). | time-step control | timestep_dt_t |
| IDTGR | INTEGER | (102) | Per-group time-step group ids (102 slots). | time-step control | timestep_dt_t |
| KDTINT | INTEGER | scalar | Global time-step integration mode. | time-step control | timestep_dt_t |

### SCR19 — Registry offsets for curves/tables/materials/properties
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr19_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 126 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| JFUNC | INTEGER | (MAXFUNC) | Function registry offsets. | curves/tables/materials/properties | registry_tables_t |
| JMAT | INTEGER | (MAXMAT) | Material registry offsets. | curves/tables/materials/properties | registry_tables_t |
| JPID | INTEGER | (MAXPID) | Property registry offsets. | curves/tables/materials/properties | registry_tables_t |
| JTAB | INTEGER | (MAXTAB) | Table registry offsets. | curves/tables/materials/properties | registry_tables_t |
| NJFUNC | INTEGER | scalar | Number of registered functions. | curves/tables/materials/properties | registry_tables_t |
| NUPARAM | INTEGER | scalar | Number of user parameters. | curves/tables/materials/properties | registry_tables_t |
| NJMAT | INTEGER | scalar | Number of registered materials. | curves/tables/materials/properties | registry_tables_t |
| NJPID | INTEGER | scalar | Number of registered properties. | curves/tables/materials/properties | registry_tables_t |
| NJTAB | INTEGER | scalar | Number of registered tables. | curves/tables/materials/properties | registry_tables_t |

### SCR19R — Geometry scratch buffer
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr19_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 126 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| BUFGEO0 | INTEGER | (BGEOSIZE) | Geometry scratch buffer. | geometry scratch | registry_tables_t |

### SCR21 — Default-card presence flag
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr21_c.inc`
**Present in**: starter only
**Usage count**: 0 (observed) source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IDEFAULT | INTEGER | scalar | Default-card presence/activation flag. | starter defaults | starter_defaults_t |

### SCR22 — Shell inertia scaling factor
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr22_c.inc`
**Present in**: starter only
**Usage count**: 2 (observed) source include-sites
**Note**: `INER_9_12` is set from shell integration-rule choices in `starter/source/starter/contrl.F` and consumed in shell inertia/mass setup (`starter/source/elements/shell/coque/cinmas.F`).

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| INER_9_12 | REAL(WP) | scalar | Shell inertia scaling factor tied to selected integration rule. | shell initialization | shell_inertia_t |

### ANIVAR1 — Per-ply effective plastic strain animation flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr25_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_EPSP | INTEGER | (MAXLAY) | Per-ply effective plastic strain animation selector array. | composite animation | animation_layers_t |

### ANIVAR2 — Per-ply stress animation flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr25_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_STRESS | INTEGER | (MAXLAY) | Per-ply stress animation selector array. | composite animation | animation_layers_t |

### ANIVAR3 — Per-ply damage animation flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr25_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_DAMA | INTEGER | (MAXLAY) | Per-ply damage animation selector array. | composite animation | animation_layers_t |

### ANIVAR4 — Per-ply strain animation flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr25_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_STRAIN | INTEGER | (MAXLAY) | Per-ply strain animation selector array. | composite animation | animation_layers_t |

### ANIVAR5 — Per-ply strain-rate animation flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr25_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_EPSDOT | INTEGER | (MAXLAY) | Per-ply strain-rate animation selector array. | composite animation | animation_layers_t |

### ANIVAR6 — Per-ply plastic work animation flags
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr25_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| ANIM_WPLA | INTEGER | (MAXLAY) | Per-ply plastic-work animation selector array. | composite animation | animation_layers_t |

### SCR25 — Ply and layered animation selections
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scr25_c.inc`
**Present in**: both (engine content differs)
**Usage count**: 78 source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| IEPSDOTALL_PLY | INTEGER | scalar | Output all ply strain-rate tensors flag. | composite animation | animation_layers_t |
| IDAMAALL_PLY | INTEGER | scalar | Output all ply damage values flag. | composite animation | animation_layers_t |
| PLY_ANIM | INTEGER | (MX_PLY_ANIM) | Packed list of selected ply ids. | composite animation | animation_layers_t |
| PLY_ANIM_STRESS | INTEGER | (MX_PLY_ANIM) | Packed list of ply stress outputs. | composite animation | animation_layers_t |
| PLY_ANIM_PHI | INTEGER | (MX_PLY_ANIM) | Packed list of ply failure index/phi outputs. | composite animation | animation_layers_t |
| PLY_ANIM_EPSP | INTEGER | (MX_PLY_ANIM) | Packed list of ply plastic-strain outputs. | composite animation | animation_layers_t |
| PLY_ANIM_STRAIN | INTEGER | (MX_PLY_ANIM) | Packed list of ply strain outputs. | composite animation | animation_layers_t |
| PLY_ANIM_EPSDOT | INTEGER | (MX_PLY_ANIM) | Packed list of ply strain-rate outputs. | composite animation | animation_layers_t |
| PLY_ANIM_DAMA | INTEGER | (MX_PLY_ANIM) | Packed list of ply damage outputs. | composite animation | animation_layers_t |
| IBRICK_STRESSALL | INTEGER | scalar | Output all brick stresses flag. | composite animation | animation_layers_t |
| IBRICK_STRAINALL | INTEGER | scalar | Output all brick strains flag. | composite animation | animation_layers_t |
| IBRICK_EPSPALL | INTEGER | scalar | Output all brick plastic strains flag. | composite animation | animation_layers_t |

### SCRY — Starter element-family counts
**File**: `/home/laurent/OpenRadioss2/starter/share/includes/scry_c.inc`
**Present in**: starter only
**Usage count**: 34 (observed) source include-sites

| Variable | Type | Dims | Inferred Purpose | Usage Domain | Proposed UDT |
|---|---|---|---|---|---|
| NUMSOL | INTEGER | scalar | Number of solid elements. | starter element counting | entity_counts_t |
| NUMQUAD | INTEGER | scalar | Number of quadrilateral elements. | starter element counting | entity_counts_t |
| NUMSHEL | INTEGER | scalar | Number of shell elements. | starter element counting | entity_counts_t |
| NUMTRUS | INTEGER | scalar | Number of truss elements. | starter element counting | entity_counts_t |
| NUMBEAM | INTEGER | scalar | Number of beam elements. | starter element counting | entity_counts_t |
| NUMSPRI | INTEGER | scalar | Number of spring elements. | starter element counting | entity_counts_t |
| NUMSH3N | INTEGER | scalar | Number of 3-node shell elements. | starter element counting | entity_counts_t |
| NUMSPHY | INTEGER | scalar | Number of SPH elements. | starter element counting | entity_counts_t |

## Proposed UDT Summary

| UDT Name | Member count | Source commons | Notes |
|---|---:|---|---|
| simulation_control_t | 190 | COM01, SCR03, SCR06, SCR06R, SCR12 | global solver control; preprocessing and versioning; reactions / decomposition; reporting and damping |
| ale_control_t | 8 | COM01, COM04, SCR03, SCRALE1 | ALE |
| entity_counts_t | 260 | COM01, COM04, COM10, SCRY | loads, groups, and BCs; model sizing and allocation; starter element counting |
| time_control_t | 87 | COM01, COM04, COM06, COM08, COM08DP, COM09, SCR02, SCR06R, SCR07, SCR11 | time integration and damping; time integration and scheduling; time integration and stability |
| parallel_sync_t | 10 | COM01, LOCKMP, SCR01 | parallel synchronization; parallel/runtime |
| noise_control_t | 9 | COM01, SCRNO_I, SCRNO_R | noise post-processing |
| airbag_control_t | 24 | COM01, COM04, COM09, SCRFS | airbag/fluid |
| contact_control_t | 15 | COM01, COM04 | contact/interfaces |
| xfem_control_t | 43 | COM01, COM04, XFEM3I, XFEMI | XFEM/crack |
| material_state_layout_t | 35 | COM01, COM04 | element/material state |
| output_control_t | 334 | COM01, COM04, COM06, COM08, COM09, SCR05, SCR06, SCR07, SCR12, SCR14, SCR16, SCR16R, SCR16_STATI, SCR16_STATR, SCR20, SCR23, SCRCUT | I/O formats and output configuration; animation output; animation/output; animation/output scheduling; section cuts; state output scheduling; state output selection; status output scheduling; status output selection |
| registry_tables_t | 28 | COM01, COM04, COM06, SCR07, SCR19, SCR19R | curves/tables/integration; curves/tables/materials/properties; geometry scratch |
| selected_counts_t | 16 | COM10 | selection and filtering |
| quadrature_tables_t | 4 | COM20 | numerical integration |
| commandline_opts_t | 16 | CMDLINE | startup and CLI parsing |
| element_work_t | 10 | SCR02, SCR08, SCR08_A | element geometry; element processing |
| task_private_t | 34 | SCR01, SCR04 | thread-private runtime scratch; thread-private scratch |
| file_io_ctrl_t | 18 | SCR07, SCR13, SCR15, SCR15I, SCR20, SCRFS | ABF/file piping; curves and fluid surfaces; file and run control; file and unit handling; file naming |
| energy_balance_t | 14 | SCR11 | energy accounting |
| deletion_control_t | 42 | SCR17, SCR17ID | element deletion and id tables; id-table sizing |
| timestep_dt_t | 21 | SCR18, SCR18R | time-step control |
| animation_layers_t | 44 | ANIVAR1, ANIVAR2, ANIVAR3, ANIVAR4, ANIVAR5, ANIVAR6, SCR25 | composite animation |
| workspace_partition_t | 12 | SCRFCI, SCRFCR | workspace sizing |
| engcards_t | 8 | COM_ENGCARDS | starter engine-card preprocessing |
| starter_defaults_t | 1 | SCR21 | starter defaults |
| shell_inertia_t | 1 | SCR22 | shell initialization |

## Variables difficult to classify

- `FTEMPVAR21` (`COM01`, engine/starter): explicit placeholder/reserved field.
- `NOTUSED` (`SCR03`, engine): reserved compatibility slot.
- `IFIF/IF01..IF05/MFIF/MF01..MF05` (`SCRFCI`/`SCRFCR`): clearly partition/offset fields, but exact sub-workspace semantics are context dependent.
- `DSNROW/DSNCOL/DSNBLOC` (`COM01`, engine): likely dimensions of a dense/sparse network structure; exact data structure deserves deeper call-graph tracing.
- `ST_INVERS` (`SCR03`, engine): version payload/packed string field; exact encoding is not obvious from the include alone.
- `TEST_POIDS` (`COM01`, starter): appears to be a weight/mass-test switch; naming is legacy/French and deserves dedicated cleanup.
- `ARCHINFO/ARCHN` (`SCR05`, starter): architecture metadata is easy to recognize, but the precise column semantics of `ARCHINFO(MAXARCH,2)` need local reader-side inspection.
- Entity-family counters such as `NUMELQ/NUMELS/NUMELC/NUMELT/...` are straightforward counts, but their exact family naming should be preserved from existing OpenRadioss terminology during refactoring rather than re-expanded heuristically.
