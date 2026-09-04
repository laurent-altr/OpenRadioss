# SPH in OpenRadioss — implementation, formulas, and a CUDA port study

> **Purpose of this document.**
> Part 1–8 describe *what the code actually does*: data structures, execution
> flow, neighbour search and the discrete formulas, all traceable to
> `file:line`. Part 9 is an engineering feasibility study. Part 10 turns it
> into a concrete implementation playbook: the required array lifetimes, the
> Fortran/C++/CUDA ownership boundary, and the order in which to port it.
>
> **Scope of the CUDA study** (agreed constraints): core SPH only — continuity
> density, renormalized gradients, stress forces, artificial viscosity —
> plus variable smoothing length and tensile-instability stabilization.
> Single MPI rank owning all particles. FP32 target. Material laws assumed at
> full `mmain` parity. Excluded: SPMD exchange, solid-to-SPH, inlets/outlets,
> thermal, XSPH velocity smoothing, contact. No validation campaign.

---

## Table of contents

1. [Scope and file inventory](#1-scope-and-file-inventory)
2. [Data model](#2-data-model)
3. [Execution flow](#3-execution-flow)
4. [Neighbour search](#4-neighbour-search)
5. [Symmetry analysis](#5-symmetry-analysis)
6. [Formulas](#6-formulas)
7. [Material-law coupling](#7-material-law-coupling)
8. [Parallelism today](#8-parallelism-today)
9. [CUDA port study](#9-cuda-port-study)
10. [C++/CUDA integration playbook](#10-ccuda-integration-playbook)
11. [Reference index](#11-reference-index)

---

## 1. Scope and file inventory

OpenRadioss implements SPH as **element type 51** (`ITY==51`). An SPH particle
is *not* a standalone entity: it is attached to a mesh node, and its force is
injected into the ordinary nodal force array. The property card is
`/PROP/TYPE34` (SPH).

- Starter: `starter/source/elements/sph/` — reading, initial smoothing length,
  group building, first neighbour search.
- Engine: `engine/source/elements/sph/` — the solver.
- MPI: `engine/source/mpi/sph/` and `engine/source/mpi/elements/spmd_sph.F`.

### 1.1 Engine file inventory

Total lines / non-comment, non-blank lines ("code"). Files marked **[core]**
are inside the agreed CUDA scope.

| File | total | code | Role |
|---|---:|---:|---|
| `weight.F` **[core]** | 121 | 54 | Cubic B-spline kernel `WEIGHT0`/`WEIGHT1` |
| `spcompl.F` **[core]** | 1424 | 517 | Kernel renormalization coefficients (`WACOMP`) |
| `spdens.F` **[core]** | 391 | 277 | Velocity gradient, strain rate, continuity density |
| `spforcp.F` **[core]** | 916 | 653 | Pair force loop, artificial viscosity, nodal stiffness |
| `spstab.F` **[core]** | 823 | 613 | Tensile-instability stabilization tensor |
| `spadah.F` **[core]** | 105 | 49 | Variable smoothing length update |
| `spstres.F` **[core]** | 328 | 218 | Constitutive driver, calls `MMAIN` |
| `spload3.F` **[core]** | 126 | 78 | Gather particle → `MVSIZ` block |
| `spback3.F` **[core]** | 76 | 29 | Scatter `MVSIZ` block → particle |
| `spoff3.F` **[core]** | 71 | 29 | Particle metadata / activity |
| `spdefo3.F` **[core]** | 74 | 28 | Strain rate → material convention |
| `spvol3.F` **[core]** | 117 | 32 | Volume from density |
| `spmall3.F` **[core]** | 94 | 46 | Particle deletion bookkeeping |
| `spreploc.F` | 142 | 90 | Corotational orthotropic frame |
| `sppro3.F` | 238 | 146 | Orthotropic projection |
| `srep2glo.F` | 314 | 244 | Local ↔ global frame transformation |
| `sphprep.F` **[core]** | 803 | 533 | Sort driver, re-search criterion |
| `sphtri.F` **[core]** | 108 | 53 | Search entry point |
| `sphtri0.F` **[core]** | 223 | 129 | Bounding box, voxel sizing |
| `sptrivox.F` **[core]** | 645 | 359 | Voxel bucket sort + candidate creation |
| `spbuc3.F` **[core]** | 455 | 301 | Neighbour-list symmetrization + truncation |
| `spclasv.F` **[core]** | 722 | 600 | Per-cycle neighbour classification, `h` reduction |
| `sph_crit_voxel.F90` **[core]** | 303 | 210 | Refined re-search criterion |
| `splissv.F` | 1001 | — | XSPH conservative velocity smoothing *(excluded)* |
| `sptemp.F` | 832 | — | Thermal: `SPGRADT`, `SPLAPLT` *(excluded)* |
| `soltosph*.F` | 2660 | — | Solid → SPH conversion *(excluded)* |
| `sponof1/2.F`, `sponfv.F`, `sponfprs.F`, `sponfro.F` | 2454 | — | Inlets / outlets *(excluded)* |
| `spsym.F`, `spsgsym.F`, `spsym_alloc.F`, `spadasm.F` | 1289 | — | Symmetry planes `/SPHBCS` *(excluded)* |
| `sph_nodseg.F`, `spgauge.F`, `spechan.F`, `spbilan.F`, `sphreq.F`, `sphres44b.F`, `spwfvis.F` | 1878 | — | Utilities, gauges, balance, restart |
| **Engine SPH total** | **18 733** | | |

**In-scope engine code: ≈ 4 800 non-comment lines** (the **[core]** rows).

Starter SPH: 3 744 lines. MPI SPH: 3 796 lines (excluded from the port).

### 1.2 Global sizing constants

| Constant | Value | Set at |
|---|---|---|
| `NISP` | 8 | `starter/source/starter/starter0.F:609` |
| `NSPBUF` | 14, or 15 if `Xi_Stab > 0` | `starter0.F:610`, `starter/source/properties/sph/hm_read_prop34.F:172` |
| `KWASPH` | 16 | `engine/source/engine/radioss2.F:498` |
| `KVOISPH` | 120 (240 with CSPH) | `starter/source/general_controls/computation/hm_read_sphglo.F:80-101` |
| `LVOISPH` | 120 | same, `:80-103` |
| `SPASORT` | 0.25 (`ALPHA2` on `/SPHGLO`) | `hm_read_sphglo.F:77,93,99` |

---

## 2. Data model

### 2.1 The central identity: an SPH particle *is* a node

```fortran
INOD = KXSP(3,N)          ! particle N  ->  node INOD
M    = NOD2SP(JNOD)       ! node JNOD   ->  particle M
```
`spforcp.F:115-116`, `spforcp.F:161`

Consequences:

- Positions and velocities are **not** stored in the SPH arrays. They are read
  from the global `X(3,NUMNOD)` and `V(3,NUMNOD)`.
- Time integration, mass, contact, boundary conditions and the global time step
  are handled by the generic nodal machinery — SPH contributes only a force and
  a nodal stiffness.
- The neighbour list `IXSP` stores **node numbers**, not particle numbers.

### 2.2 `KXSP(NISP=8, NUMSPH)` — integer particle metadata

The authoritative description is the comment block in
`starter/source/elements/reader/hm_read_sphcel.F:69-77`:

| Slot | Meaning | Written | Read |
|---|---|---|---|
| 1 | **unused** (`INUTILISE`) | — | — |
| 2 | `±NG` — signed group number; `<= 0` ⟹ particle inactive/deleted | `spmall3.F:29-58`, `forintp.F:1261` | `spforcp.F:113-114,161`, `spclasv.F:302` |
| 3 | `INOD` — associated node number | `starter/.../spinih.F:117` | everywhere |
| 4 | `NVOIS` — number of **active** neighbours (`d < h_i+h_j`) | `spbuc3.F:444`, `spclasv.F:328,541` | `spforcp.F:156`, `spdens.F:175`, `spcompl.F:175`, `spadah.F:74` |
| 5 | number of **real candidates** retained by the bucket search; equivalently the right boundary of the real portion of `IXSP(:,N)` | `spbuc3.F:428` | `spforcp.F:529`, `spclasv.F:110` |
| 6 | number of active neighbours in the **symmetric (ghost)** part | `spclasv.F:407,602` | `spforcp.F:528`, `spcompl.F:208` |
| 7 | number of **candidates** in the symmetric part | `spsym.F` | `spclasv.F:110,228` |
| 8 (`NISP`) | **user cell ID**; also the sort tie-break key | starter | `spbuc3.F:204`, `sptrivox.F:314`, `spadah.F:100` |

### 2.3 `IXSP(KVOISPH, NUMSPH)` — the neighbour list

**Terminology:** the engine symbol is `KVOISPH`, not `KVOIS`. `KVOISPH` is a
single, global **hard capacity per particle** (120 by default, §1.2); it is
not itself a neighbour list and it is not a particle's actual neighbour count.
`IXSP(:,N)` is the list, `KXSP(4,N)` is its active-real count, and
`KXSP(5,N)` is its real-candidate count. `LVOISPH` is a separate soft target
used by the adaptive-`h` overflow policy.

A single fixed-stride column per particle, holding four different kinds of
entry. The layout is:

```
index:  1 .......... KXSP(4,N) .......... KXSP(5,N) ....... KXSP(5,N)+KXSP(7,N)
        |<-- active real -->|<-- reserve -->|<--- symmetric ghosts --------->|
                                             |<-KXSP(6,N)->|
```

| Range | Content |
|---|---|
| `1 .. KXSP(4,N)` | active real neighbours, `d < h_i + h_j` — **this is what the physics loops use** |
| `KXSP(4,N)+1 .. KXSP(5,N)` | "reserve" candidates found within the search margin but currently outside the support; promoted/demoted every cycle by `SPCLASV` |
| `KXSP(5,N)+1 .. +KXSP(6,N)` | active symmetric-plane ghost neighbours |
| `.. +KXSP(7,N)` | reserve ghost candidates |

**Entry encoding** (`spforcp.F:160-161`, `:336-346`, `:531-537`):

| Value | Meaning |
|---|---|
| `JNOD > 0` (real part) | node number; particle is `M = NOD2SP(JNOD)` |
| `JNOD < 0` (real part) | **remote** (other MPI rank) particle, index `NN = -JNOD` into `XSPHR` |
| `JS > 0` (ghost part) | `SM = JS/(NSPCOND+1)`, `NC = MOD(JS,NSPCOND+1)`, then `ISPSYM(NC,SM)` |
| `JS < 0` (ghost part) | ghost of a remote particle, via `ISPSYMR` |

### 2.4 `SPBUF(NSPBUF=14/15, NUMSPH)` — real per-particle state

| Slot | Meaning | Written | Read |
|---|---|---|---|
| 1 | **`h`** — smoothing length ("particle diameter"). Support of the kernel for a pair is `h_i + h_j` | `starter/.../spinih.F:110,143,163`, `spadah.F:98`, `spclasv.F:197` | everywhere |
| 2 | **`rho`** — density | `spdens.F:380`, `starter/.../spinit3.F:153` | `spforcp.F:121,175` |
| 3, 4 | **unused** (no read or write anywhere in starter or engine) | — | — |
| 5, 6, 7 | position `(x,y,z)` **at the last neighbour search** | `sphtri.F:92-94` | `sphprep.F:272-274` |
| 8 | `h` **at the last neighbour search** | `sphtri.F:95`, `spclasv.F:198` | `sphprep.F:374,410` |
| 9 | legacy restart-4.4b compatibility flag | `sphres44b.F:76` | `sphres44b.F:73` |
| 10 | **accumulated** artificial-viscosity energy | `forintp.F:1185`, `spwfvis.F:72` | `thsph.F:188` |
| 11 | **instantaneous** artificial-viscosity power `WVIS` | `spforcp.F:525,716,903` | `spwfvis.F:72` |
| 12 | **mass** (volume at input time if slot 13 = 2) | `starter/.../spinit3.F:154` | `spforcp.F:251,253` |
| 13 | input flag: `1` = mass given, `2` = volume given | `starter/.../spinit3.F:142-150` | `starter/.../spinih.F:91-99` |
| 14 | `h0` — initial smoothing length | `starter/.../spinih.F:164` | `spadah.F:79,93-94` |
| 15 | `DP/H` — normalized nearest-neighbour distance in the **initial** configuration, `2*sqrt(DVOIS(1))`. Allocated only when `Xi_Stab > 0` | `starter/.../spclasv.F:103` | `spstab.F:125-126` |

### 2.5 `WA(KWASPH=16, NUMSPH)` — the working array, **reused per phase**

This is the single most important porting hazard: **the same slot means
different things at different points in the cycle.**

| Slot | Density phase (`spdens.F`) | After `SPBACK3` | Force phase (`spforcp.F`) |
|---|---|---|---|
| 1–6 | strain rate `Exx,Eyy,Ezz,Exy,Eyz,Exz` | stress `SIG(1..6)` | stress, read only |
| 7 | `Eyx` | `STI` (element stiffness) | **accumulator** for nodal stiffness |
| 8 | `Ezy` | `SSP` (sound speed) | read only |
| 9 | `Ezx` | `SSP_EQ` | read only |
| 10 | `rho` (old density, in / out) | — | **accumulator** `F_x` |
| 11 | `DELTAX` (= `h`) | — | **accumulator** `F_y` |
| 12 | `MUMAX` | — | **accumulator** `F_z` |
| 13 | `DIVV` | preserved | read only (viscosity switch), then `spadah` |
| 14 | `ROTV` | preserved | read only (viscosity switch) |
| 15 | — | `DIE` (accumulated) | — |
| 16 | never used | never used | never used |

Writers/readers: `spdens.F:95-97,227-235,374-386`; `spback3.F:58-73`;
`spload3.F:31-72`; `spforcp.F:518-520,709-711,897-899`, `:334`.

The ordering constraints are stated explicitly in the source
(`forintp.F:258-266`):

> *"load wa (1:14*numsph), which must not be crushed before SPSTRES … Divv =
> Wa(13,1:NUMSPH) and Rotv = Wa(14,1:NUMSPH) must not be overwritten before
> the calculation of forces (SPFORC) … Divv must not be crushed before spadah"*

### 2.6 `WACOMP(16, *)` — kernel renormalization coefficients

Produced by `SPCOMPL`, consumed by `SPDENS`, `SPFORCP`, `SPLISSV`, `SPTEMP`.

| Slot | Meaning | Written |
|---|---|---|
| 1 | `alpha_i` — zeroth-order normalization | `spcompl.F:248` |
| 2–4 | **always zero** in the current code (legacy `BETAx/y/z` of the old order-1 scheme, still visible in the commented block at `spcompl.F:1130-1139`) | `spcompl.F:258-260` |
| 5–7 | `∂alpha_i/∂x, ∂alpha_i/∂y, ∂alpha_i/∂z` | `spcompl.F:252-256` |
| 8–16 | `-L_i^{-1}` — inverse first-order correction matrix, row-major `(xx,yx,zx, xy,yy,zy, xz,yz,zz)` | `spcompl.F:417-425`; zeroed for order-0 particles at `:261-269` |

`WACOMPR` is the identical 16-slot layout for remote particles.

### 2.7 `STAB(7, NUMSPH+NSPHR+NSPHSYM+1)`

Allocated at `forintp.F:705`.

| Slot | Meaning |
|---|---|
| 1, 2, 3 | `xx, yy, zz` of the symmetric artificial-stress tensor |
| 4, 5, 6 | `xy, yz, xz` |
| 7 | scalar weight; `0` disables stabilization for that particle |

Usage convention confirmed at `spforcp.F:265-270`.

### 2.8 `XSPHR(:, NSPHR)` — remote particle mirror

Layout documented in `engine/share/modules/sphbox.F` and used at
`spforcp.F:337-349`:

| Slot | 1 | 2 | 3:5 | 6 | 7 | 8 | 9:11 | 12 | 13 | 14 |
|---|---|---|---|---|---|---|---|---|---|---|
| | local marker | `h` | `x,y,z` | cell ID | `rho` | mass | `vx,vy,vz` | group | `KXSP(2)` | part |

### 2.9 Auxiliary arrays

| Name | Meaning |
|---|---|
| `NOD2SP(NUMNOD)` | node → particle (0 if the node is not an SPH particle) |
| `WASPACT(NSPHACT)` | compacted list of **active** particles; the force-assembly and `spadah` loops iterate over this |
| `WSP2SORT(NSP2SORT)` | compacted list of particles participating in the neighbour search |
| `IPARTSP(NUMSPH)` | particle → `/PART` index (gives material and property) |
| `ISPSYM(NSPCOND,*)`, `XSPSYM(3,*)`, `VSPSYM(3,*)` | symmetry-plane ghost tables |
| `ISPCOND(NISPCOND,*)`, `XFRAME` | symmetry-plane definitions (`/SPHBCS`) |

### 2.10 `engine/share/includes/sphcom.inc`

| Symbol | Meaning |
|---|---|
| `NUMSPH` | local SPH particle count |
| `NISP`, `NSPBUF`, `KWASPH` | strides of `KXSP`, `SPBUF`, `WA` |
| `KVOISPH` | hard capacity of a neighbour-list column |
| `LVOISPH` | soft target for the number of *active* neighbours |
| `NSPCOND`, `NISPCOND`, `NSPHSYM`, `NSPHSYMR` | symmetry conditions and ghost counts |
| `NSPHIO`, `NISPHIO`, `LVSPHIO` | inlet/outlet |
| `ISPHBUC` | "a full re-search is required" flag, persistent across cycles |
| `SPASORT`, `SPATRUE`, `SPAOLD` | search-margin parameters |
| `ISPHRED`, `NSPHACT`, `NSP2SORT` | reduction flag, active count, sort-set count |
| `NSPHSOL`, `FIRST_SPHSOL`, `ITSOL2SPH`, `SOL2SPH_FLAG` | solid→SPH |
| `DBUCS`, `XBMIN..ZBMAX`, `NBOX/NBOY/NBOZ` | voxel grid |

### 2.11 Which list is which?

The list names have different roles and must not be collapsed accidentally:

| Name | Population | Lifetime | Purpose |
|---|---|---|---|
| `NUMSPH` | all local SPH particles, including inactive ones | run | capacity/index domain `N=1..NUMSPH` |
| `KXSP(2,N)>0` | live particle | until deletion | authoritative activity test |
| `WSP2SORT(1:NSP2SORT)` | particles that require search/classification work | rebuilt by `SPHPREP` | worklist for neighbour topology operations |
| `IXSP(:,N)` | candidate neighbours of particle `N` | after a full search, then reclassified | fixed-capacity topology cache |
| `KXSP(4,N)` / `KXSP(6,N)` | active real / active ghost prefix of `IXSP(:,N)` | every cycle | the prefixes consumed by physics loops |
| `WASPACT(1:NSPHACT)` | all live particles which participate in the force-side passes | rebuilt/updated by `SPCLASV` | compact worklist for stabilization, force assembly and `SPADAH` |

`KXSP(5,N)` is both a count and the **inclusive right-boundary index** of the
real candidate region: because Fortran uses one-based indices, the last real
candidate is at `IXSP(KXSP(5,N),N)`. It is *not* the number of active
neighbours; that is `KXSP(4,N)`. Under the proposed first milestone
(one rank, no `/SPHBCS`), only the first two regions are present:

```text
IXSP(1 : KXSP(4,N), N)                 active local physical neighbours
IXSP(KXSP(4,N)+1 : KXSP(5,N), N)       local reserve candidates
```

The negative remote entries and the two symmetry-ghost regions must be
rejected by the initial GPU feature gate, not silently interpreted as local
particle indices.

### 2.12 `WA` liveness: the real phase contract

`WA` looks like one particle-state array but is an ABI between consecutive
Fortran phases. The table below is more useful to a port than the slot map in
§2.5 because it states the last safe consumer of each meaning.

| Interval | `WA` meaning that is live | Writer | Must be consumed before |
|---|---|---|---|
| start of `FORINTP` → `SPDENS` | slot 10 = old `rho` | inline at `forintp.F:287-289` | `SPDENS` reads it as `RHOI` (`spdens.F:113`) |
| after `SPDENS` → `SPSTRES` | 1:9 = full velocity-gradient/strain tensor; 10 = old `rho`; 11 = `h`; 12 = `MUMAX`; 13 = `DIVV`; 14 = `ROTV` | `SPDENS` | `SPLOAD3` / `SPSTRES` |
| after `SPBACK3` → `SPFORCP` | 1:6 = stress; 7 = material `STI`; 8 = `SSP`; 9 = `SSP_EQ`; 10:14 retain the density-phase values; 15 accumulates `DIE` | `SPBACK3` | `SPFORCP` requires stress, sound speeds, `MUMAX`, `DIVV` and `ROTV` |
| after `SPFORCP` → nodal assembly | 7 = accumulated nodal stiffness; 10:12 = accumulated force `(Fx,Fy,Fz)`; 15 = thermal contribution when enabled; `SPBUF(11)` = viscosity power | `SPFORCP` | inline assembly to `A`/`STIFN`/`FTHE` or the Parith skyline |
| after nodal assembly → end of cycle | 7 = SPH time-step input; 13 = `DIVV` for `SPADAH` | `SPFORCP` / `SPDENS` | time-step/deletion block and `SPADAH` |

`SPDENS` first zeroes slots 1:9, 11 and 12, but deliberately leaves slot 10
untouched so it remains the old density (`spdens.F:95-113`). `SPBACK3` then
overwrites the material-result part of the buffer (`spback3.F:58-73`), and
`SPFORCP` reuses the stiffness and force slots. A C++ implementation must use
distinct named arrays (`strain9`, `stress6`, `sound_speed`, `force3`,
`stiffness`, `divv`, `rotv`, `old_rho`, ...) internally. It should translate
to/from these legacy slots only at the Fortran boundary.

---

## 3. Execution flow

### 3.1 Position within the engine cycle

Two entry points in `RESOL`:

| Line | Call | When |
|---|---|---|
| `resol.F:4324` | `SPHPREP` | after `FORINT`, before `FORINTP` — neighbour search, symmetrization, `WACOMP` |
| `resol.F:4370` | `FORINTP` | the SPH physics itself |
| `resol.F:6821` | `SPLISSV` | after integration — XSPH velocity smoothing |

Everything between is inside `engine/source/elements/forintp.F`.

### 3.2 `SPHPREP` — preparation

```
SPHPREP                                        sphprep.F
├─ SOLTOSPH_ON1                    (if NSPHSOL/=0)         :170
├─ SPONOF1                         (inlets/outlets)        :197
├─ globalize ISPHBUC over MPI                              :218
├─ if ISPHBUC==0:                                          :248
│    ├─ SPECHAN            real/ghost particle swap        :250
│    ├─ reduce (min,max) of  X - SPBUF(5:7)  ->  MAJORD    :271-330
│    ├─ criterion  sqrt(1+SPATRUE)*SPBUF(8,N) - MAJORD <= SPBUF(1,N)
│    │                                    -> ISORTSP=1     :371-378
│    └─ sph_crit_voxel   refine MAJORD with a 15^3 voxel grid,
│                        re-test the criterion             :396-420
├─ if ISORTSP==1:                                          :443
│    ├─ SPHTRI0    global bbox + voxel size                :458
│    ├─ SPHTRI     -> SPBUC3 -> SPTRIVOX  (rebuild lists)  :464
│    │              then save SPBUF(5:8) = X, h            sphtri.F:92-95
│    └─ SPMD_SPHGAT  compact/renumber remote neighbours    :487
├─ ISPHBUC = ISORTSP                                       :496
├─ if ISORTSP==1:  SPSYM_ALLOC, SPSYMP, SPCLASV            :533-583
│                  (build symmetry ghosts, classify)
├─ SPADASM0 / SPADASM   ghost positions & velocities       :684,720
├─ SPCLASV              re-classify EVERY cycle            :695
└─ SPCOMPL              renormalization coefficients       :793
```

**`SPCLASV` at line 695 runs unconditionally, every cycle.** This is what keeps
`KXSP(4,N)` (the active neighbour count) correct between two full searches.

### 3.3 `FORINTP` — the SPH physics

Phases are separated by `MY_BARRIER`. Within a phase, element groups are
distributed dynamically over OpenMP threads through a `NGDONE` counter guarded
by `lockon.inc`/`lockoff.inc`.

| # | Phase | Routine | Line | Reads | Writes |
|---|---|---|---|---|---|
| 0 | save old density | inline | `:287-289` | `SPBUF(2)` | `WA(10)` |
| 1 | thermal gradient *(if therm)* | `SPGRADT` | `:423` | `T`, `WACOMP` | `WGRADT` |
| 2 | thermal Laplacian *(if therm)* | `SPLAPLT` | `:497,516` | `WGRADT` | `WLAPLT` |
| 3 | **density & strain rate** | `SPDENS` | `:579` | `X,V,SPBUF,WACOMP` | `WA(1:14)`, `SPBUF(2)` |
| 4 | inlet/outlet density reset | `SPONFRO` | `:607` | | `SPBUF(2)` |
| 5 | **constitutive** | `SPSTRES` → `MMAIN` | `:663` | `WA(1:12)`, `ELBUF_TAB` | `WA(1:9,15)`, `ELBUF_TAB` |
| 6 | solid→SPH | `SOLTOSPHP` | `:695` | | |
| 7 | stabilization weight | `SPSTABW` | `:713` | `SPBUF(15)`, `IXSP` | `STAB(7)` |
| 8 | ghost stress propagation | `SPSGSYM` | `:758` | `WA(1:6)` | `WASIGSM` |
| 9 | stabilization tensor | `SPSTABS` | `:800` | `WA(1:6)` | `STAB(1:6)` |
| 10 | **forces** | `SPFORCP` | `:837` | `WA(1:9,13,14)`, `WACOMP`, `STAB` | `WA(7,10:12)`, `SPBUF(11)` |
| 11 | **nodal assembly** | inline | `:861-909` | `WA(7,10:12,15)` | `A`, `STIFN`, `FTHE` / `FSKYI` |
| 12 | gauges | `SPGAUGE` | `:1190` | | |
| 13 | SPH particle time step | inline | `:1241-1278` | `WA(7)`, `MS` | may delete particles |
| 14 | **variable `h`** | `SPADAH` | `:1299` | `WA(13)`, `SPBUF(1,14)` | `SPBUF(1)` |

### 3.4 Nodal assembly — the offload boundary

```fortran
DO NS=ITASK+1,NSPHACT,NTHREAD
  N=WASPACT(NS)
  MYADRN = KWASPH*(N-1)
  INOD   = KXSP(3,N)
  A(1,INOD)  = A(1,INOD)  + WA(MYADRN+10)
  A(2,INOD)  = A(2,INOD)  + WA(MYADRN+11)
  A(3,INOD)  = A(3,INOD)  + WA(MYADRN+12)
  STIFN(INOD)= STIFN(INOD)+ WA(MYADRN+7)
ENDDO
```
`forintp.F:885-893` (Parith/OFF). With `IPARIT/=0` the same values go into the
`FSKYI`/`ISKY` skyline instead (`forintp.F:896-909`).

The present Fortran implementation exposes a clean engine boundary:
**particle contribution in** = `WA(7,10:12,15)`; **nodal contribution out** =
`A`, `STIFN` (+ `FTHE`) or `FSKYI`/`ISKY`. A first C++/CUDA physics core should
return the former, particle-indexed values and retain this assembly loop in
Fortran. Its wider input set is `X`, `V`, `MS` plus the SPH state listed in
§2.12; this keeps contact and the generic nodal machinery untouched.

### 3.5 One local particle's journey through a cycle

The following is the smallest useful mental model for the first CUDA
milestone. It deliberately excludes the negative `IXSP` entries (MPI) and
the symmetry-ghost suffix; §10.2 makes those explicit feature gates.

```text
particle N
  │ KXSP(2,N)>0, node=KXSP(3,N)
  ▼
SPHPREP
  │ may rebuild candidates IXSP(:,N), then saves X/h in SPBUF(5:8,N)
  │ always classifies candidates: KXSP(4,N) becomes active-real count
  │ recomputes WACOMP(:,N)
  ▼
FORINTP / SPDENS
  │ X,V,SPBUF,WACOMP,active IXSP prefix
  │ -> strain9, old rho, h, MUMAX, DIVV, ROTV
  ▼
FORINTP / SPSTRES / MMAIN                 (retained Fortran initially)
  │ packs the values in MVSIZ blocks
  │ -> material state in ELBUF_TAB, stress6, STI, SSP, SSP_EQ, DIE
  ▼
FORINTP / stabilization / SPFORCP
  │ stress6 + WACOMP + DIVV/ROTV + active IXSP prefix
  │ -> force3, stiffness, viscosity power
  ▼
FORINTP / nodal assembly                  (retained Fortran initially)
  │ force3/stiffness -> A(:,INOD), STIFN(INOD), or FSKYI/ISKY
  ▼
time-step/deletion -> SPADAH updates h -> next SPHPREP
```

The state boundary after each arrow is a phase barrier, not merely a
convenience. In particular, neither the material phase nor a CUDA force
launch may start until `SPDENS` has completed for all particles, and
`SPADAH` must follow force assembly because it consumes `DIVV`. `SPCLASV`
must finish before any phase iterates the active `IXSP(1:KXSP(4,N),N)`
prefix. This is the ordering to preserve even after `WA` has been replaced
by named C++ arrays.

---

## 4. Neighbour search

### 4.1 When is a full search done?

Never every cycle. `ISPHBUC` (persistent, globalized over MPI at
`sphprep.F:218`) forces a search when set — e.g. at `TT==0`
(`sphprep.F:222`), or when a particle is deleted (`spmall3.F`, `forintp.F:1261`
both set `ISPHBUC=1`).

Otherwise the displacement criterion applies (`sphprep.F:365-378`):

```
MAJORD  = 0.5 * || (max - min) of  (X - SPBUF(5:7)) ||       ! bound on relative motion
SPALINR = sqrt(1 + SPATRUE)                                   ! search margin, SPATRUE=0.25 by default
if   SPALINR * SPBUF(8,N) - MAJORD  <=  SPBUF(1,N)   for any N  ->  re-search
```

Read as: the list was built with radius `SPALINR·h_sort`; particles may have
moved by at most `MAJORD`; a re-search is needed as soon as the guaranteed
covered radius no longer exceeds the *current* `h`. `MAJORD` is a global
scalar, which is very pessimistic, so `sph_crit_voxel.F90` recomputes a
localized `majord_vox` on a 15³ voxel grid and re-tests (`sphprep.F:396-420`).

### 4.2 The search itself

`SPHTRI0` (`sphtri0.F`) computes the global bounding box and the voxel size,
inflated by the margin: `DBUC = DBUC*sqrt(1+SPATRUE)*1.0001` (`sphtri0.F:142`).

`SPTRIVOX` (`sptrivox.F`) then:

1. inserts every particle into a linked list per voxel (`VOXEL`/`NEXT_NOD`,
   `sptrivox.F:193-234`);
2. for each particle `J`, scans the voxel box of half-width
   `AAA = 2·h_J·sqrt(1+SPASORT)` (`sptrivox.F:274-292`);
3. for each candidate `JS`, applies a **strict ordering filter**
   (`sptrivox.F:313-314`):

```fortran
IF(SPBUF(1,JS) > SPBUF(1,J) .OR.
.  (SPBUF(1,JS) == SPBUF(1,J) .AND. KXSP(8,JS)>=KXSP(8,J))) GOTO 200
```

   i.e. `J` only records `JS` when `h_JS < h_J` (ties broken by cell ID);
4. accepts on distance with the pair radius (`sptrivox.F:333-349`):

```fortran
AAA = SPBUF(1,J)+SPBUF(1,JS)      ! = h_J + h_JS
BBB = AAA * ALPHA_MARGE
IF(D2 > BBB*BBB) GOTO 200
DVOIS(NVOIS) = D2/(AAA*AAA)       ! normalized squared distance
```

The ordering filter of step 3 is what makes the box of step 2 sufficient:
since `h_JS ≤ h_J`, we have `h_J + h_JS ≤ 2·h_J`.

The result is a **half list**: each pair is stored exactly once, on the side of
the larger `h`.

### 4.3 Explicit symmetrization

`SPBUC3` then runs a block that is literally titled
`C     SYMETRISE VOISINS` (`spbuc3.F:169-171`):

1. count, per target particle, how many reverse entries it will receive
   (`spbuc3.F:196-216`);
2. prefix-sum into `IV`, allocate `IAUX` (`spbuc3.F:233-258`);
3. scatter the reverse entries — for every stored pair `(N → M)` with
   `h_M < h_N`, append `INOD` to `M`'s list (`spbuc3.F:280-298`);
4. do the same for remote pairs via `KXSPR`/`IXSPR` (`spbuc3.F:300-317`).

Each particle's candidate list is then the concatenation of its own half-list
(`KXSP(5,N)` entries) and the reverse entries (`NVOISS`), and distances are
recomputed for all of them (`spbuc3.F:328-402`).

### 4.4 Overflow: truncation and `h` reduction

Two mechanisms:

**(a) Hard truncation** (`spbuc3.F:364-427`):

```fortran
IF(NVOIS>KVOISPH)THEN
  IREDUCE=1
  KREDUCE(N)=1
END IF
...
IF(KREDUCE(N)/=0 .AND. NVOIS > KVOISPH) THEN
  CALL MYQSORT(NVOIS,DVOIS,JPERM,IERROR)     ! sort by normalized distance
  DO K=1,KVOISPH ; JVOIS(K)=JSTOR(JPERM(K)) ; ENDDO
  DK=DVOIS(KVOISPH)
C Choice of cells to keep such that distance < DK to avoid parallelization problems
  NVOIS=0
  DO K=1,KVOISPH ; IF(DVOIS(K)<DK) NVOIS=NVOIS+1 ; ENDDO
END IF
NVOIS=MIN(NVOIS,KVOISPH)
```

**(b) Adaptive `h` reduction** (`spclasv.F:104-199`), the preferred path: when
a particle has more than `LVOISPH` active neighbours, `SPCLASV` computes

```fortran
IF(KREDUCE(N) >= 10) DWA(N)=SQRT(DVOIS(LVOISPH))
...
SPBUF(1,N)=MIN(SPBUF(1,N),DWA(N)*SPBUF(1,N))
SPBUF(8,N)=SPBUF(1,N)
```

i.e. it *shrinks the particle's smoothing length* until only `LVOISPH`
neighbours fit. Since the pair radius is `h_i + h_j`, shrinking `h_i` removes
the pair from **both** lists consistently.

### 4.5 Per-cycle re-classification

`SPCLASV` (`spclasv.F:290-333`) re-partitions the candidate list every cycle:

```fortran
DMS = SPBUF(1,N)+SPBUF(1,M)
IF (KXSP(2,M)/=0 .AND. DD<DMS*DMS) THEN
  NVOIS1=NVOIS1+1 ; MWA(NVOIS1)=JNOD          ! active
ELSE
  NVOIS2=NVOIS2+1 ; MWA(KVOISPH+NVOIS2)=JNOD  ! reserve
END IF
...
KXSP(4,N)=NVOIS1
```

Both conditions — `d < h_i + h_j` and "the neighbour is alive" — are
**symmetric in `(i,j)`**. The active entries are then re-sorted by particle
number `for conservation PARITH/ON` (`spclasv.F:336-345`), which is what makes
the force sum reproducible.

---

## 5. Symmetry analysis

This section answers the question directly: *is the OpenRadioss SPH
neighbourhood unsymmetric — if A sees B, does B see A?*

### 5.1 The neighbour relation **is** symmetric, by construction

| Ingredient | Symmetric? | Evidence |
|---|---|---|
| pair smoothing length `h_ij = (h_i+h_j)/2` | ✅ | `spforcp.F:177`, `spdens.F:149`, `spcompl.F:322` |
| kernel support `2·h_ij = h_i + h_j` | ✅ | `weight.F:53,58` |
| candidate distance test `d ≤ (h_i+h_j)·sqrt(1+α)` | ✅ | `sptrivox.F:333-349` |
| active test `d < h_i + h_j` **and** neighbour alive | ✅ | `spclasv.F:299-302` |
| half-list build + explicit reverse scatter | ✅ | `sptrivox.F:313-314` + `spbuc3.F:169-317` |
| `h` reduction on overflow (shrinks `h_i`, affects both sides) | ✅ | `spclasv.F:197-198` |

So the *topology* is symmetric. The single residual exception is the hard
`KVOISPH` truncation (`spbuc3.F:405-427`), which is per-particle and can in
principle drop a pair from one side only. The code visibly tries to mitigate
this — the `DVOIS(K) < DK` filter is annotated *"to avoid parallelization
problems"* — but `DK` is a per-particle threshold, so it does not fully
guarantee mutual consistency. In practice this path is reached only after the
adaptive `h` reduction has failed to bring the count under `LVOISPH`.

### 5.2 The **forces**, however, are *not* pairwise antisymmetric

This is the real effect, and it is deliberate. OpenRadioss does not use the
classical symmetric-pair form
`F_ij = -m_i m_j (σ_i/ρ_i² + σ_j/ρ_j²)·∇W_ij`. It uses a **renormalized
(Randles–Libersky style) SPH** in which every particle carries its own
correction operator.

In `spforcp.F:184-192`, the gradient seen by `i` is corrected with `i`'s own
coefficients:

```fortran
WGRAD(1)=WGRDX*ALPHAI+WGHT*ALPHAXI
.        +WGRDX*BETAXXI+WGRDY*BETAXYI+WGRDZ*BETAXZI
```

and in `spforcp.F:223-228` the *"noyau conjugué"* (conjugate kernel) is built
with `j`'s coefficients:

```fortran
WGRD(1)=-WGRDX*ALPHAJ+WGHT*ALPHAXJ
.       -WGRDX*BETAXXJ-WGRDY*BETAXYJ-WGRDZ*BETAXZJ
```

Because `alpha_i ≠ alpha_j` and `L_i^{-1} ≠ L_j^{-1}`, we have
`∇W_i(j) ≠ -∇W_j(i)`, and therefore

```
F_ij  ≠  -F_ji
```

for the **stress** part. Only the artificial-viscosity part is explicitly
antisymmetrized, with the comment `C FV(j,i)=-FV(i,j)` (`spforcp.F:299-305`):

```fortran
WGRDX=(WGRAD(1)-WGRD(1))*HALF
FVX  =-FACT*WGRDX
```

**Consequences.**

- Linear momentum is conserved only approximately for the stress term (exactly
  for the viscous term).
- In exchange, the scheme is **first-order consistent**: it reproduces linear
  velocity fields exactly, which classical symmetric SPH does not. This is the
  standard trade-off in solid SPH, and it is why the correction exists.
- Boundary particles, where `alpha_i` deviates most from 1, are where the
  asymmetry is largest.

### 5.3 The force loop is pure **gather**

Every accumulation in `SPFORCP` targets the *owner* particle only:

```fortran
WA(10,N)=WA(10,N)+FX
WA(11,N)=WA(11,N)+FY
WA(12,N)=WA(12,N)+FZ
```
`spforcp.F:518-520` (real neighbours), `:709-711` (ghosts), `:897-899` (remote
ghosts). Nothing is ever scattered to `J`.

The formulation therefore does roughly **2× the pair work** of a symmetric
scheme, but in return:

- no atomics, no write conflicts, no colouring;
- no Parith/ON skyline inside the SPH kernel;
- bitwise-deterministic results for a fixed neighbour ordering (which
  `spclasv.F:336-345` guarantees by sorting on particle number).

For a GPU port this is close to ideal, and it is the single most favourable
property of this implementation.

---

## 6. Formulas

Notation: `r = |x_i - x_j|`, `h ≡ DIJ = (h_i + h_j)/2`, `q = r/h`,
`V_j = m_j/ρ_j`.

### 6.1 Kernel — cubic B-spline (M4), 3D

`weight.F:53-65` (`WEIGHT0`), `:98-114` (`WEIGHT1`):

```
        ⎧ (1/(π h³)) · (1 - 3/2 q² + 3/4 q³)          0 ≤ q ≤ 1
W(r,h) =⎨ (1/(4π h³)) · (2 - q)³                      1 < q ≤ 2
        ⎩ 0                                           q > 2
```

Radial derivative, as implemented (`WPRIMR` already contains the `1/r` factor
of the chain rule in the second branch):

```
                ⎧ (1/(π h³)) · (-3 + 9/4 q) / h²      0 ≤ q ≤ 1
dW/dr · (1/r) = ⎨ -(3/4) (2-q)²/(π h³) / (h² r)       1 < q ≤ 2
                ⎩ 0
```

and `WGRAD(a) = WPRIMR · (x_a,i - x_a,j)` (`weight.F:116-118`), i.e.
`∇_i W_ij`.

Note the support is `2h = h_i + h_j`, which is exactly the acceptance radius
used by the search (§4).

### 6.2 Renormalization — `SPCOMPL`

**Order 0** (`spcompl.F:166-256`). The self-contribution is included first
(`spcompl.F:169-170`):

```
Σ⁰_i  = V_i W(0,h_i) + Σ_j V_j W_ij
alpha_i = 1 / max(ε, Σ⁰_i)                                  spcompl.F:247
∇alpha_i = -alpha_i² · Σ_j V_j ∇W_ij                        spcompl.F:252-256
```

**Order 1** (`spcompl.F:297-425`, active when the property requests
`IORDER==1`, flag `SPH_IORD1` set at `spcompl.F:123-128`). The moment matrix is
accumulated at `spcompl.F:328-336`:

```
L_i[a][b] = Σ_j V_j · (∂_a W_ij) · (x_b,i - x_b,j)
```

then inverted through transposed cofactors and the determinant
(`spcompl.F:404-415`) and stored **negated**:

```
WACOMP(8:16, i) = -L_i⁻¹                                    spcompl.F:417-425
```

> **Observation for a port.** Since `∂_a W_ij = W'(r)·(x_a,i - x_a,j)`, the
> matrix is actually
> `L_i[a][b] = Σ_j V_j W'(r) · Δx_a · Δx_b`, which is **symmetric**. The code
> accumulates and inverts all 9 components independently and does not exploit
> this. A GPU implementation can use 6 accumulators and a symmetric 3×3
> inverse.

**The corrected gradient** actually used by every physics kernel
(`spdens.F:169-192`, `spforcp.F:184-192`, `sptemp.F`):

```
∇̃W_i(j)  =  alpha_i ∇W_ij  +  W_ij ∇alpha_i  +  (-L_i⁻¹) · ∇W_ij
```

and the conjugate form used for `j` (`spforcp.F:223-228`) replaces
`∇W_ij → -∇W_ij` and `alpha_i, L_i → alpha_j, L_j`.

### 6.3 Velocity gradient and strain rate — `SPDENS`

With `VJX = V_j (v_x,i - v_x,j)` etc. (`spdens.F:193-195`), accumulated over
neighbours (`spdens.F:199-207`):

```
D_ab  =  -Σ_j V_j (v_a,i - v_a,j) · ∇̃W_i(j)_b
```

A second-order-in-`Δt` correction is applied (`spdens.F:209-226`, with
`DT1D2 = Δt/2`):

```
Exx = Dxx - Δt/2 (Dxx² + Dyx² + Dzx²)
Eyy = Dyy - Δt/2 (Dyy² + Dzy² + Dxy²)
Ezz = Dzz - Δt/2 (Dzz² + Dxz² + Dyz²)
Exy = Dxy - Δt/2 (Dxx·Dxy + Dyx·Dyy + Dzx·Dzy)      ( = Eyx correction )
Eyz = Dyz - Δt/2 (Dyy·Dyz + Dzy·Dzz + Dxy·Dxz)
Exz = Dxz - Δt/2 (Dzz·Dzx + Dxz·Dxx + Dyz·Dyx)
Eyx = Dyx - Δt/2 (Dxx·Dxy + Dyx·Dyy + Dzx·Dzy)
Ezy = Dzy - Δt/2 (Dyy·Dyz + Dzy·Dzz + Dxy·Dxz)
Ezx = Dzx - Δt/2 (Dzz·Dzx + Dxz·Dxx + Dyz·Dyx)
```

stored to `WA(1:9,N)` (`spdens.F:227-235`).

Divergence and rotation (`spdens.F:250-252`, `:378-386`):

```
DIVV = Exx + Eyy + Ezz
rot_x = Σ_j (VJY ∂_z W̃ - VJZ ∂_y W̃),  and cyclic
ROTV  = |rot|
```

### 6.4 Density — continuity equation, not summation

`spdens.F:378-381`:

```
Δρ  = -DIVV · ρ_i
ρ_i ← max(ε, ρ_i + Δρ · Δt)
```

Volume is reconstructed from density (`spvol3.F:72-74`):

```
V_A = (ρ₀/ρ_A)·V₀      V_N = (ρ₀/ρ_N)·V₀      ΔV = V_N - V_A
```

### 6.5 Artificial viscosity

`spforcp.F:286-298`:

```
μ_ij  = h_ij · ((v_i-v_j)·(x_i-x_j)) / (|x_i-x_j|² + 0.01 h_ij²)
μ_ij  = min(μ_ij, 0)                                    ! compression only
μ_ij  = μ_ij · (f_i + f_j)/2                            ! Balsara-type switch
   with  f_i = |DIVV_i| / (|DIVV_i| + ROTV_i)
c̄     = (SSP_i + SSP_j)/2
Π_ij  = (q_a μ_ij² - q_b c̄ μ_ij) · 2/(ρ_i + ρ_j)
```

`q_a = GEO(14,IPROP)`, `q_b = GEO(15,IPROP)` (`spforcp.F:131-132`).

### 6.6 Forces

Stress part (`spforcp.F:241-258`), with `σ_i = WA(1:6,i)`:

```
A = σ_i · ∇̃W_i(j)
B = -σ_j · ∇̃W_j(i)          (the conjugate kernel)
F_stress = V_i V_j (A + B)
```

Viscous part (`spforcp.F:299-305`):

```
F_visc = -m_i m_j Π_ij · (∇̃W_i(j) - ∇̃W_j(i))/2
```

Stabilization part (`spforcp.F:262-277`), active when
`STAB(7,i) ≠ 0 and STAB(7,j) ≠ 0`:

```
W̄   = W_ij · h_ij³
w̄   = (STAB(7,i) + STAB(7,j))/2
WI  = W̄⁴ · w̄
C   = S_i · ∇̃W_i(j),   D = -S_j · ∇̃W_j(i)      with S = STAB(1:6)
F_stab = V_i V_j · WI · (C + D)
```

Total, accumulated on `i` only (`spforcp.F:515-520`):

```
WA(10:12, i) += F_stress + F_stab + F_visc
```

and the artificial-viscosity power (`spforcp.F:523-525`):

```
SPBUF(11,i) += min(0, ½ F_visc·(v_i - v_j))
```

### 6.7 Nodal stiffness (only if `NODADT/=0` or `I7KGLO/=0`)

`spforcp.F:307-334`:

```
STII  = V_j · m_i · SSP_EQ_i² · V_j |∇̃W_i(j)|²
STIJ  = V_i · m_j · SSP_EQ_j² · V_i |∇̃W_j(i)|²
STIJ  = 2 (STII + STIJ)
c_ij  = m_i m_j |Π_ij| · |∇̃W_i - ∇̃W_j|/2 / (|(v_i-v_j)·(x_i-x_j)|/r)
ζ     = c_ij / sqrt(2 · STIJ · m_i)
s     = sqrt(1+ζ²) - ζ
STIJ  = STIJ / s²
WA(7,i) += STIJ · (1 + WI)
```

### 6.8 Stabilization tensor — `SPSTAB`

**Weight** (`spstab.F:122-131`). `DD = SPBUF(15,N)` is the normalized
nearest-neighbour distance `DP/H` in the *initial* configuration
(`starter/.../spclasv.F:103`: `SPBUF(15,N)=2·sqrt(DVOIS(1))`), defaulting to
`2/3`:

```fortran
CALL WEIGHT0(0,0,0, DD,0,0, 1.0, WGHT)      ! W(DD) with h=1
STAB(7,N) = Xi_Stab / max(ε, WGHT⁴)
```

`Xi_Stab` is the `/PROP/TYPE34` field, stored via `SET_U_GEO(7,·)`
(`starter/source/properties/sph/hm_read_prop34.F:161-166`). It also triggers
`NSPBUF=15`. `STAB(7,N)` stays `0` unless the particle has at least one
**active** neighbour (`spstab.F:98-121`).

**Tensor** (`spstab.F:219-259` and three parallel blocks for remote, ghost, and
remote-ghost particles at `:286-319`, `:376-406`, `:458-487`). A full symmetric
3×3 eigen-decomposition of the stress is performed by `SPH_VALPVEC`, then:

```
R_k = -max(0, σ_k)                 ! non-zero only for compressive principal stresses
STAB(1:6) = Σ_k R_k · (e_k ⊗ e_k)  ! rotated back to global axes
```

The code notes that the eigenvalues must be computed in double precision even
in a single-precision build (`spstab.F:228-229`) — relevant for the FP32 target.

### 6.9 Variable smoothing length — `SPADAH`

`spadah.F:78-98`, run at the very end of the cycle over the active list:

```
h_new = h · (1 + DIVV · Δt · COEFF)
if H_FLAG == 3:   h_new = clamp(h_new, HMIN·h₀, HMAX·h₀)
SPBUF(1,N) = h_new
```

with `COEFF = GET_U_GEO(8)`, `H_FLAG = GET_U_GEO(9)`,
`HMIN/HMAX = GET_U_GEO(10:11)`. Modes (`spadah.F:87-90`):

| `H_FLAG` | Behaviour | `COEFF` |
|---|---|---|
| 0 | 3D dilatation | 1/3 |
| 1 | 1D dilatation | 1 |
| 2 | constant `h` | 0 |
| 3 | 3D dilatation, bounded by `hmin·h₀ < h < hmax·h₀` | 1/3 |

### 6.10 Time step

**Material/element step**, `MDTSPH` (`mdtsph.F:127-135`):

```
QXMATER = CNS1·SSP + VISI·(2·VIS + CNS2)/max(ε, ρ·Δx)
QX      = q_b·SSP + q_a·MUMAX + QXMATER
Δt      = Δx / max(ε, QX + sqrt(QX² + c²))
SSP_EQ  = max(ε, QXMATER + sqrt(QXMATER² + c²))
```

with `Δx = DELTAX = h` (`spdens.F:375` sets `WA(11,N)=SPBUF(1,N)`) and
`MUMAX = max_j(-μ_ij)` (`spdens.F:239-248`).

Thermal limit if `JTHE/=0` (`mdtsph.F:138-160`):
`Δt ← min(Δt, ½ Δx² · C_p / k(T))`.

Element stiffness (`mdtsph.F:163-166`): `STI = 2ρV/Δt²`.

**Nodal step and particle deletion** (`forintp.F:1241-1278`, only when
`NODADT==1`):

```fortran
DTX = DTFAC1(51)*SQRT(TWO*MS(INOD)/MAX(EM20,WA(KWASPH*(N-1)+7)))
IF(DTX <= DTMIN1(51)) THEN
   IDTMIN(51)==1 -> stop
   IDTMIN(51)==2 -> GBUF%OFF(K)=0 ; KXSP(2,N)=0 ; ISPHBUC=1   ! delete particle
   IDTMIN(51)==5 -> MSTOP=2
```

Note that deleting a particle sets `ISPHBUC=1`, forcing a full neighbour
re-search on the next cycle.

---

## 7. Material-law coupling

### 7.1 `SPSTRES` — the constitutive driver

`spstres.F` is a thin adapter between the particle-indexed SPH arrays and the
`MVSIZ`-blocked, group-oriented material interface used by solids:

```
SPOFF3     :199   metadata: NGEO, NGL, MXT, NC1, OFF               spoff3.F:34-53
SPLOAD3    :205   gather WA(1:12,N) and SPBUF(2,N) into MVSIZ arrays
SPVOL3     :216   volume from density
SPDEFO3    :223   D4=Dxy+Dyx, D5=Dyz+Dzy, D6=Dxz+Dzx,
                  WZZ=Δt/2(Dyx-Dxy), WYY=Δt/2(Dxz-Dzx), WXX=Δt/2(Dzy-Dyz)
SROTA3     :230   stress rotation
SPREPLOC   :235   orthotropic frame update
MMAIN      :251   >>> the material law <<<
SPMALLB3   :296   deletion bookkeeping (sets KXSP(2,N)=0, ISPHBUC=1)
SPBILAN    :312   energy balance
SPBACK3    :322   scatter SIG, STI, SSP, SSP_EQ, DIE back to WA
```

`SPLOAD3` gather (`spload3.F:31-72`):

```fortran
RHON(I)=SPBUF(2,N)
DXX(I)=WA(1,N) ... DZX(I)=WA(9,N)
RHOA(I)=WA(10,N) ; DELTAX(I)=WA(11,N) ; MUMAX(I)=WA(12,N)
```

`SPBACK3` scatter (`spback3.F:58-73`):

```fortran
WA(1:6,N)=SIG(I,1:6) ; WA(7,N)=STI(I) ; WA(8,N)=SSP(I)
WA(9,N)=SSP_EQ(I)    ; WA(15,N)=WA(15,N)+DIE(I)
```

SPH uses a single layer / single integration point: `BUFLY(1)%LBUF(1,1,1)`,
`ILAY=1` hardwired (`spstres.F:96-104`). `GBUF` fields used: `OFF`, `RHO`,
`VOL`, `EINT`, `SIG`, `GAMA`, `EINTTH`, `dt`; `LBUF%PLA`.

### 7.2 Which material laws support SPH

The authority is the starter compatibility check
(`starter/source/materials/mat/check_mat_elem_prop_compatibility.F:72-90, 154-166`):
element type 51 and `/PROP/TYPE34` both require `MAT_PARAM(IMAT)%PROP_SPH /= 0`,
which a law sets by calling `INIT_MAT_KEYWORD(MATPARAM,"SPH")`
(`starter/source/materials/mat/init_mat_keyword.F:274-275`).

**35 numbered laws declare SPH compatibility** (36 reader files — law 6 has
two, `hm_read_mat06.F` and `hm_read_mat06_keps.F`):

```
0, 1, 3, 5, 6, 10, 12, 13, 14, 21, 22, 23, 24, 28, 33, 34, 35, 36, 38,
40, 41, 42, 49, 66, 70, 72, 75, 79, 93, 97, 102, 103, 105, 109, 121
```

…plus the user-material path (laws 29–31, 99, 200), which goes through
`usermat_solid` then `mulaw` (`mmain.F90:2323-2398`,
`starter/source/materials/mat/matuser/hm_read_mat_user29_31.F:163`).

Reproduce the list with:

```
grep -rl 'INIT_MAT_KEYWORD(MATPARAM,"SPH")' starter/source/materials/
```

`MDTSPH` is invoked from the `jsph /= 0` branch of many law routines
(`m1law.F:157`, `m2law.F:194`, `m22law.F`, `m24law.F`, `mmain.F90` law
branches) and is documented as callable from `m1law`, `m1lawi`, `m1lawtot`,
`m2law`, `m22law`, `m24law`, `mmain`, `mulaw`, `sboltlaw`, `usermat_solid`
(`mdtsph.F:1-13`).

### 7.3 Size of the material dependency

| Item | Lines |
|---|---:|
| `mmain.F90` (dispatcher) | 2 907 |
| `mulaw.F90` (user-law path) | 3 068 |
| `mdtsph.F` (in `engine/source/materials/mat_share/`) | 221 |
| Engine source of the 35 SPH-capable law directories (`engine/source/materials/mat/mat0NN`…) | **33 290** |
| **Total material dependency** | **≈ 39 500** |

The law routines depend on global includes (`implicit_f.inc`, `mvsiz_p.inc`,
`com08_c.inc`, `param_c.inc`, `impl1_c.inc`, `units_c.inc`, `comlock.inc`) and
on modules `table_mod`, `sensor_mod`, `matparam_def_mod`, `fail_param_mod`,
`eosmain_mod`, `nlocal_reg_mod`, `glob_therm_mod`, `dt_mod`, … Many laws use
tabulated functions (`NPC`/`TF`/`finter`), failure models and sensors.

---

## 8. Parallelism today

### 8.1 OpenMP

Element groups are distributed dynamically. Each phase in `forintp.F` uses the
same pattern:

```fortran
NGDONE = 1
CALL MY_BARRIER
100 CONTINUE
#include "lockon.inc"
    IF(NGDONE>NGROUP) THEN ; GOTO 101 ; ENDIF
    NG = NGDONE ; NGDONE = NG + 1
#include "lockoff.inc"
    ... process group NG ...
    GOTO 100
101 CONTINUE
CALL MY_BARRIER
```

Some passes instead stride over the active/sort lists
(`DO NS=ITASK+1,NSPHACT,NTHREAD`).

Because `SPFORCP` only writes to its own particle's slots, **no locks are
needed in the force loop**; locks appear only around the queue counter and the
global min/max reductions in `sphprep.F:314-328`.

### 8.2 Determinism (Parith/ON)

With `IPARIT/=0`, SPH forces go into the `FSKYI`/`ISKY` skyline instead of `A`
(`forintp.F:896-909`), and the neighbour list is sorted by particle number
before the force loop (`spclasv.F:336-345`, comment: *"Sorting effective
particles according to no particle for conservation PARITH/ON"*). Together
these make the result independent of thread and rank count.

### 8.3 SPMD (excluded from the port)

For completeness, the distributed layer is:

| File | Lines | Role |
|---|---:|---|
| `spmd_sph.F` | 2 510 | `SPHGETW/GETV/GETT/GETG/GETWA/GETSTB/GETD/GETH/GETISPH` — exchange of `SPBUF`, `WA`, `WACOMP`, `STAB` for remote particles |
| `spmd_sphvox.F` | 410 | remote voxel exchange during the search |
| `spmd_sphgat.F` | 346 | compaction / renumbering of remote neighbour entries |
| `spmd_sphgetv.F` | 271 | remote velocity/mass gather |
| `spmd_sptool.F` | 171 | helpers |
| `spmd_spamaj.F` | 88 | global reduction of the search margin |

Remote particles appear as negative entries in `IXSP` and are read from
`XSPHR`/`WACOMPR`. With a single rank, `NSPHR = 0` and all of this collapses:
in `SPFORCP`, `SPDENS` and `SPCOMPL` the entire `JNOD < 0` branch becomes dead
code — which is roughly **half of `spforcp.F`** (the four near-duplicate
neighbour loops are local / local-remote / ghost / ghost-remote).

---

## 9. CUDA port study

### 9.1 What makes this implementation unusually GPU-friendly

1. **Gather-only force loop.** Accumulation is exclusively into the owner
   particle's slots (`spforcp.F:518-520`). No atomics, no colouring, no
   skyline, no reduction tree. One CUDA thread (or warp) per particle writes
   its own registers and stores once.
2. **Fixed-stride neighbour storage.** `IXSP(KVOISPH, NUMSPH)` with
   `KVOISPH = 120` is already a dense matrix. Transposed to
   `IXSP[k*N + i]` it gives perfectly coalesced neighbour loads.
3. **Barrier-separated phases.** The `MY_BARRIER` structure of `forintp.F`
   maps 1:1 onto CUDA kernel launches — the existing decomposition *is* the
   GPU decomposition.
4. **Narrow interface to the rest of the solver** (§3.4): in `X, V, MS`, out
   `A, STIFN`.
5. **Potentially amortized neighbour search.** The re-search criterion (§4.1)
   permits the candidate topology to survive multiple cycles. The actual
   interval is model-dependent and must be measured.
6. **No pointer chasing in the physics kernels** — everything is flat arrays
   indexed by integers.

The main structural obstacles are the `MVSIZ`-blocked material interface
(§9.5) and the per-phase reuse of `WA` (§2.5).

### 9.2 Proposed device data layout (SoA)

| Device array | Type | Size | Source |
|---|---|---|---|
| `d_x, d_y, d_z` | `float` | `N` | gathered from `X(3,INOD)` |
| `d_vx, d_vy, d_vz` | `float` | `N` | gathered from `V(3,INOD)` |
| `d_h, d_h0, d_rho, d_mass` | `float` | `N` | `SPBUF(1,14,2,12)` |
| `d_sig` | `float` | `6N` | `WA(1:6)` |
| `d_eps` | `float` | `9N` | `WA(1:9)` (strain rate; aliases `d_sig`) |
| `d_alpha, d_dalpha` | `float` | `N`, `3N` | `WACOMP(1)`, `WACOMP(5:7)` |
| `d_Linv` | `float` | `9N` (or `6N`, see §6.2) | `WACOMP(8:16)` |
| `d_stab` | `float` | `7N` | `STAB(1:7)` |
| `d_divv, d_rotv, d_mumax, d_ssp, d_ssp_eq, d_sti` | `float` | `N` | `WA(13,14,12,8,9,7)` |
| `d_force` | `float` | `3N` | `WA(10:12)` |
| `d_nbr` | `int32` | `KVOISPH·N`, transposed | `IXSP` |
| `d_nnbr` | `int32` | `N` | `KXSP(4)` |
| `d_active` | `uint8` | `N` | `KXSP(2) > 0` |

Structure-of-arrays throughout, with the neighbour matrix stored
**neighbour-major** (`d_nbr[k*N + i]`) so that a warp processing 32 consecutive
particles reads 32 consecutive `int32` per neighbour slot.

Because the neighbour list stores *node* numbers, either keep a device copy of
`NOD2SP` or — better — renumber the lists once per search to store particle
indices directly and drop the indirection from the inner loop.

Also worth doing at port time: **stop aliasing `WA`**. Give strain rate, stress
and force distinct arrays. The Fortran aliasing exists to save memory on
1990s-era machines; at `N = 10⁶` the whole extra cost is a few tens of MB.

### 9.3 Kernel decomposition

| # | Kernel | Grid | Work per particle | Notes |
|---|---|---|---|---|
| K1 | `gather_state` | `N` | O(1) | `X,V` → SoA |
| K2 | `neighbour_search` | see §9.4 | O(27·occupancy) | only when the criterion fires |
| K3 | `classify` | `N` | O(`KVOISPH`) | replaces `SPCLASV`: recompute `d < h_i+h_j` and the alive test, compact active entries |
| K4 | `compl` | `N` | O(`nnbr`) | `alpha`, `∇alpha`, `L`, symmetric 3×3 inverse |
| K5 | `dens` | `N` | O(`nnbr`) | velocity gradient, strain rate, `DIVV`, `ROTV`, `MUMAX`, `ρ` update |
| K6 | `constitutive` | `N` | O(1) | material law — see §9.5 |
| K7 | `stab_w` | `N` | O(`nnbr`) | scalar weight |
| K8 | `stab_s` | `N` | O(1) | symmetric 3×3 eigen-decomposition |
| K9 | `forces` | `N` | O(`nnbr`) | **the hot kernel** |
| K10 | `scatter_nodal` | `N` | O(1) | `A`, `STIFN` |
| K11 | `dt_and_h` | `N` | O(1) | time step, particle deletion, `SPADAH` |

K3–K9 are pure per-particle gathers with no inter-thread communication, so each
is a single trivially-parallel launch. K9 is the natural hot-kernel candidate:
each active neighbour requires a kernel evaluation, two corrected-gradient
applications, a tensor-vector product and viscosity work. Whether the resulting
implementation is compute- or bandwidth-bound must be measured on the target
GPU and model.

Suggested mapping for K9: **one warp per particle**, each lane handling a
strided subset of the neighbour list, followed by a warp shuffle reduction of
the 3 force components + `WA(7)`. This keeps register pressure low despite the
~40 live scalars in the inner body (`spforcp.F:85-105` declares that many) and
gives coalesced access to the neighbour-major list.

### 9.4 Neighbour search on the GPU

Two viable options.

**(a) Port the existing voxel algorithm.** `sptrivox.F` builds per-voxel linked
lists (`VOXEL`/`NEXT_NOD`) — a serial construction that maps badly to a GPU.
Replace with the standard GPU cell list: compute a cell index per particle,
sort particle indices by cell key (`cub::DeviceRadixSort`), then a binary
search / atomic histogram to build `cellStart`/`cellEnd`. This replaces the
serial linked-list construction with a standard parallel primitive; its runtime
must be profiled against the target model.

**(b) Keep the half-list + symmetrization structure** (§4.2–4.3). This can be
kept verbatim and is even attractive on GPU: the `h_JS < h_J` filter halves the
candidate work, and the symmetrization pass is a counting + prefix-sum +
scatter — i.e. exactly `cub::DeviceScan` plus one scatter kernel. But note that
after symmetrization the reverse entries are appended in a thread-dependent
order (`spbuc3.F:280-317`); to preserve determinism the list must be sorted by
particle index afterwards, which the CPU code also does
(`spclasv.F:336-345`).

Recommendation: **(a) for the cell list, then (b)'s explicit symmetrization**,
because the symmetric list is what makes the `d < h_i+h_j` criterion
self-consistent (§5.1).

`KVOISPH` overflow handling should keep the adaptive-`h` mechanism
(`spclasv.F:197`) rather than hard truncation, since it is symmetric and
GPU-friendly (a per-particle `sqrt` of the `LVOISPH`-th sorted distance — use a
per-warp partial sort or a `nth_element`-style selection rather than a full
`MYQSORT`).

### 9.5 The material laws — the dominant cost

This is where "full `mmain` parity" collides with reality:

- **35 numbered laws** are SPH-compatible (§7.2), plus user laws 29–31.
- **33 290 lines** of engine law code, plus `mmain` (2 907) and `mulaw`
  (3 068).
- The laws are written against `MVSIZ`-blocked arrays, global `COMMON` blocks
  via `#include`, and pull in tables (`finter`), sensors, failure models and
  EOS.

Three options, in increasing order of cost:

| Option | Description | Consequence |
|---|---|---|
| **A. Offload pair loops only** | K4, K5, K7, K9 on GPU; K6 (constitutive) stays in Fortran on the CPU | Requires a device→host→device round trip of material inputs and outputs every cycle. In the simple 15-float exchange this is `60N` bytes (`60 MB` at `N=10⁶`, FP32). It may erase end-to-end speed-up unless transfer and CPU material time are measured and overlapped. **It preserves full law parity.** |
| **B. Port a small law set** | e.g. LAW1 (elastic), LAW2 (Johnson–Cook), LAW3/4, LAW6 (hydro), LAW49 | Roughly 2–4 k new CUDA lines for a small selected set. Supports only models using those laws; all other models need the CPU material path. |
| **C. Full parity on GPU** | Port all 35 laws + `mmain` + `mulaw` + tables + failure + EOS | ~39 500 lines to translate, with the global-`COMMON` and `MVSIZ` restructuring. This is the single largest work package by an order of magnitude and dwarfs the SPH kernels themselves. |

**The honest conclusion**: "core SPH on CUDA" is a bounded, tractable project
(≈ 4 800 lines of Fortran in scope, of which roughly half is the
single-rank-dead remote branches). "Core SPH on CUDA *with full `mmain`
parity*" is not — the material library is ~8× the size of the SPH solver.

A pragmatic path is **A → B**: start with the constitutive update on the host
(full parity, correctness reference), then move the hot laws to the device one
at a time, keeping the CPU path as a fallback selected per material.

### 9.6 FP32 implications

The build is normally double precision (`my_real` = `double precision` unless
`SINGLE_PRECISION`). Moving the SPH kernels to FP32 raises specific issues,
several of which the existing code already anticipates:

| Risk | Where | Comment |
|---|---|---|
| Guard constants | `EM20` (1e-20) appears 128× across 17 files in `engine/source/elements/sph/` (38 in `spcompl.F`, 37 in `spforcp.F`); `EM30` in `spstab.F:126,131`; `EP20` in `sphprep.F` | `EM20` and `EM30` are themselves representable in FP32 (smallest normal `~1.18e-38`), so the guards still *work*. The hazard is their **products**: `EM20²=1e-40` falls into the subnormal range (precision loss, and flushed to zero under `-ftz`), and `EM30²=1e-60` underflows to exactly zero. Any guarded quantity that is subsequently squared, or any `MAX(EM20,·)` feeding a denominator that is then squared, must be re-tuned — `1e-18f` is a safer floor. Audit every squared use. |
| Kernel normalization | `INVPI/(H*H*H)` (`weight.F:49`) | For small `h` (say `1e-4` m) this is `~3e11`; multiplied by `V_j` and summed over 120 neighbours it stays in range, but `h³` itself underflows FP32 for `h < 1e-12`. Rescale to a normalized `q` formulation. |
| Eigen-decomposition | `spstab.F:228-229` | The code **already says** eigenvalues must be computed in double precision even in a single-precision build. Keep K8 in FP64 or use a well-conditioned analytic symmetric 3×3 solver. |
| Matrix inversion | `spcompl.F:414-415` — `DET=1./DET` with no conditioning guard | `L_i` is near-singular for particles with few neighbours (free surfaces). In FP32 this will produce garbage where FP64 merely produces large numbers. **Add a determinant/condition threshold and fall back to order-0.** This is arguably a latent robustness issue in the existing code too. |
| Accumulation error | up to 120 neighbours summed per particle | Use Kahan or FP32 pairwise (warp-shuffle) reduction; a naive sequential FP32 sum over 120 terms loses ~3–4 digits. |
| Determinism | Parith/ON contract (§8.2) | A warp-shuffle reduction with a fixed neighbour order *is* deterministic, so the contract can be preserved — but results will not be bit-comparable to the FP64 CPU run, and **restart files will not be interchangeable**. |
| Time step | `mdtsph.F:127-135` | `DTX` feeds the global time step; an FP32 `DTX` that is slightly optimistic destabilizes the whole model. Compute the time-step reduction in FP64. |

Recommendation: **mixed precision** — FP32 for positions, velocities, kernel
evaluation and force accumulation; FP64 for the renormalization inverse, the
eigen-decomposition, and the time-step reduction.

### 9.7 Data movement, single-rank

There is no single, valid per-cycle byte count: it depends on which phases have
moved. The force-only MVP needs Fortran-produced geometry, physical state,
material results, correction fields, rates and (if enabled) stabilization as
inputs; it returns **particle** force/stiffness/WVIS for the existing Fortran
nodal assembly. Once `SPCOMPL`/`SPDENS` and stabilization move, their fields
become device-resident and the transfer contract shrinks. §10.7 specifies the
three contracts explicitly.

The topology is the allocation that must not cross the bus after upload:
`IXSP` at `120 × 10⁶ × 4 B = 480 MB` for `N=10⁶`. The CUDA context holds it
until CPU topology changes force a new upload; a standalone device-integrated
solver could instead avoid all per-cycle transfers except I/O snapshots.

If the port is a **standalone** solver (particles integrated on the device
too), the only transfers are I/O snapshots, and the problem disappears.

### 9.8 Work packages

Ordered by dependency. Sizes are relative complexity, not calendar estimates.

| WP | Content | In-scope Fortran | Risk |
|---|---|---|---|
| **WP0** | Reference extraction: dump `X,V,SPBUF,KXSP,IXSP,WA,WACOMP,STAB` at a chosen cycle from a real model; build a replay harness | — | low |
| **WP1** | SoA device layout, host↔device gather/scatter, de-alias `WA` | §2.5, §9.2 | low |
| **WP2** | Kernel + corrected gradient (K4): `weight.F` + `spcompl.F` local branches | 571 | **medium** — the `L⁻¹` conditioning problem in FP32 |
| **WP3** | Density / strain rate (K5): `spdens.F` local branches | 277 | low |
| **WP4** | Force kernel (K9): `spforcp.F` local + ghost branches, warp reduction | 653 | medium — register pressure, reduction determinism |
| **WP5** | Cell-list neighbour search + symmetrization + classification (K2, K3): `sptrivox`, `spbuc3`, `spclasv`, `sphprep` criterion, `sph_crit_voxel` | 1 932 | **high** — largest single block, plus the `KVOISPH` overflow policy |
| **WP6** | Stabilization (K7, K8): `spstab.F` local branches | 613 | medium — symmetric eigensolver in mixed precision |
| **WP7** | Time step + variable `h` + deletion (K11): `mdtsph`, `spadah`, `forintp` DT block | ~260 | low |
| **WP8** | Constitutive coupling (K6), **Option A**: keep `MMAIN` on the host, build the `MVSIZ` bridge | 460 (`spstres`,`spload3`,`spback3`,`spoff3`,`spdefo3`,`spvol3`,`spmall3`) | medium — the round-trip cost of §9.7 |
| **WP9** | Constitutive on device, **Option B**: 4–6 hot laws | ~3 000 (new CUDA) | medium |
| **WP10** | Constitutive on device, **Option C**: full `mmain` parity | **~39 500** | **very high — dominates everything else** |
| **WP11** | Integration into the engine: ISO_C_BINDING layer, `/GPU` option, CPU fallback per part | — | medium |

**In-scope SPH Fortran total (WP2–WP8): ≈ 4 800 lines**, of which a substantial
fraction is the `JNOD < 0` remote branches that disappear entirely under the
single-rank assumption. The corresponding CUDA is likely to be *smaller* than
the Fortran, because the four near-duplicate neighbour loops in `spforcp.F`
(local / local-remote / ghost / ghost-remote) collapse to one.

**Against that, WP10 alone is ~8× the entire SPH solver.** Any realistic plan
must either accept Option A/B or treat the material-library port as a separate,
much larger programme.

### 9.9 Things to fix rather than reproduce

Items found while reading the code that a rewrite should not carry over:

1. **`WA` slot aliasing** (§2.5) — a persistent source of ordering constraints,
   documented only in comments (`forintp.F:258-266`).
2. **Unsymmetric storage of a symmetric matrix** — `L_i` is provably symmetric
   (§6.2) but stored and inverted as a general 3×3: 9 accumulators and a full
   cofactor inversion instead of 6 and a symmetric solve.
3. **`DET=1./DET` without conditioning check** (`spcompl.F:415`) — dangerous in
   FP32 at free surfaces.
4. **`IF (H==ZERO) STOP`** inside the kernel function (`weight.F:49,94`) — an
   unconditional `STOP` in the innermost loop; must become a validated
   precondition.
5. **`GOTO`-based loop control** in `spforcp.F` (`DO 10 ... GOTO 10`) and
   `sptrivox.F` (`GOTO 200`) — trivially replaced by `continue`.
6. **`SPBUF(3,4)` are dead slots**, and `WA(16)` is never used.
7. **The `a revoir !!!!!!!` comment at `sptrivox.F:272`** — flagged by the
   authors themselves, immediately above the search-box computation. The box
   is correct *given* the `h_JS < h_J` filter, but the coupling between the two
   is implicit and fragile.

---

## 10. C++/CUDA integration playbook

This section answers the implementation question rather than merely describing
the existing code. **Source facts** are cited. Everything labelled
**recommendation** is a proposed migration design, not an existing OpenRadioss
interface.

### 10.1 Architecture decision: retain the engine; offload particle physics

The correct first boundary is **not** `MMAIN`. `SPSTRES` is already a
Fortran-native adapter: it gathers particle fields into `MVSIZ` blocks
(`SPLOAD3`), derives material inputs (`SPVOL3`, `SPDEFO3`, rotations), calls
`MMAIN`, applies deletion/energy bookkeeping, then scatters stress and
time-step outputs through `SPBACK3` (§7.1; `spstres.F:199-325`). `MMAIN` also
mutates `ELBUF_TAB` material/failure/EOS history.

**Recommendation — retain these components in Fortran initially:**

| Retained Fortran responsibility | Why it stays host-side in the first version |
|---|---|
| `RESOL` / `FORINTP` phase ordering and OpenMP barriers | It owns the solver cycle and establishes the `WA` liveness contract (§3, §2.12). |
| `SPSTRES`, `MMAIN`, `MULAW`, and all `ELBUF_TAB` state | 35 SPH-compatible laws and their shared Fortran state are the dominant porting cost (§7). |
| contact, generic nodal integration, global time-step policy | SPH is only one contributor to nodal `A`/`STIFN`; the engine already combines all contributors. |
| final nodal / Parith assembly | The existing loop writes the particle result into `A`/`STIFN` or `FSKYI`/`ISKY` (§3.4). |
| deletion, restart, output and energy bookkeeping | `SPMALLB3` can change `KXSP(2,N)` and force a rebuild; output reads material state (§7.1). |
| MPI, symmetry, inlets/outlets, thermal, conversion and XSPH | These are explicitly outside the initial single-rank core scope. |

```text
Fortran engine
  RESOL / SPHPREP / FORINTP / SPSTRES / MMAIN / nodal assembly
           │                                      ▲
           │ plain C-compatible, packed views     │ particle force/stiffness
           ▼                                      │ and selected state
  C++ host adapter ───── opaque context ───── CUDA local-particle kernels
  feature gate, packing, topology version,      no COMMON, MPI, material
  error handling and CPU fallback               history, contact or output
```

The C++ adapter must never dereference a Fortran `COMMON` block or interpret
`ELBUF_TAB`. It packs named, contiguous views on entry, and unpacks only the
specified results on return. `engine/source/mpi/output/node_checksum.F:54`
shows the project’s `ISO_C_BINDING` precedent; a thin `bind(C)` Fortran shim is
therefore the appropriate integration point.

### 10.2 Initial eligibility gate and deliberate exclusions

The first implementation is a **local physical-particle** path, not a partial
implementation that silently processes unsupported entries. Enable it only
when all conditions below hold; otherwise invoke the unchanged CPU SPH path.

| Feature | Initial decision | Reason / gate |
|---|---|---|
| MPI remote particles | exclude | Require one MPI process (`NSPMD=1`); no negative `IXSP` entries or `XSPHR`/`WACOMPR`. |
| `/SPHBCS` symmetry | exclude | Require no configured symmetry conditions or ghost particles; do not treat its encoded suffix as ordinary neighbours. |
| inlets/outlets | exclude | Require `NSPHIO=0`; these change activity and reset state in the preparation/physics flow. |
| solid-to-SPH conversion | exclude | Require `NSPHSOL=0`; conversion changes particle topology. |
| thermal and XSPH | exclude | Require their SPH passes disabled, until their state fields have explicit device contracts. |
| Parith/ON | defer | Start Parith/OFF. Add skyline output only after fixed-order, particle-result comparisons pass. |
| contact and generic integration | retain Fortran | These consume/produce the global nodal arrays around the SPH contribution. |
| material laws | retain Fortran | Keeping `SPSTRES`/`MMAIN` preserves compatibility with every supported law. |
| precision | native solver precision first | FP32 remains a later performance target with the separate safeguards in §9.6. |

Check this capability mask at initialization and before every device step. A
mid-run deletion, mode activation, topology change, restart request, or device
failure must cause a defined resynchronization or CPU fallback — never a
best-effort execution with stale neighbour indices.

### 10.3 The first thing to port: `SPFORCP` local-real force as C++ CPU code

**Recommendation:** first write a scalar/parallel **C++ CPU reference** for
only the local-real neighbour loop of `SPFORCP`, then port that exact function
to CUDA. Do not start by porting the neighbour search or the material laws.

Why this is the best first vertical slice:

1. It is the hot physics loop and operates as a pure gather: a particle writes
   only its own force/stiffness (`spforcp.F:515-520,709-711,897-899`).
2. Fortran can still prepare a known-good active neighbour prefix, correction
   data, density/rate data, stress and sound speed before this call.
3. It has a crisp, per-particle oracle: force `(Fx,Fy,Fz)`, stiffness and
   viscosity power. Results can be compared before touching nodal assembly,
   time integration, neighbour topology or material history.
4. The local branch uses only positive, physical neighbours. Under §10.2 it
   eliminates the remote and symmetry-specific loops without changing the
   local force formulation.

The C++ function should accept **named fields**, not a transposed copy of
`WA`:

| Input group | Equivalent legacy source |
|---|---|
| particle geometry / kinematics: `x[3]`, `v[3]`, `h`, `rho`, `mass` | `X`, `V`, `SPBUF(1,2,12)` |
| active local topology: `nbr_particle[k]`, `active_count` | `IXSP(1:KXSP(4,N),N)` after node→particle conversion |
| material result: `stress[6]`, `sti`, `ssp`, `ssp_eq` | `WA(1:9,N)` after `SPBACK3` |
| viscosity / rate state: `mumax`, `divv`, `rotv` | `WA(12:14,N)` from `SPDENS` |
| corrected-kernel state: `alpha`, `grad_alpha[3]`, `linv[9]` | `WACOMP(1,5:16,N)` |
| stabilization state, when enabled | `STAB(1:7,N)` |
| read-only part/property parameters | `IPARTSP` plus the property/material values read by `SPFORCP` |

It returns:

```text
force[3]        -> legacy WA(10:12,N)
stiffness        -> legacy WA(7,N)
viscosity_power -> legacy SPBUF(11,N)
```

For the *first* comparison deck, disable tensile stabilization and use
constant `h`; then add the already-implemented CPU `STAB` input before
porting the stabilization producers. This makes the force reference small
without changing its force accumulation semantics.

### 10.4 State ownership: source of truth versus device mirror

Ownership changes by migration stage. Initially **Fortran remains the source
of truth for every field**; the CUDA context is only a mirror. Once a phase
moves to the device, its derived outputs may remain resident, but the adapter
must still be able to export them to the legacy representation.

| Field class | Source of truth through force-only CUDA | Device representation / later owner |
|---|---|---|
| particle identity, node mapping, activity: `KXSP(2:3)`, `NOD2SP`, `IPARTSP` | Fortran | immutable/mirrored topology; convert node numbers to zero-based particle indices once per topology generation |
| candidate topology: `IXSP`, `KXSP(4:7)`, `WSP2SORT`, `WASPACT` | Fortran / `SPHPREP` | initially upload the active local prefix; device owns candidate storage only after the search/classification stage is ported |
| physical SPH state: `h`, `rho`, mass, `h0`, accumulated viscosity energy | Fortran `SPBUF` | separate SoA arrays; `h`/`rho` become device-resident only after their producing phases move |
| kernel correction: `WACOMP` | Fortran `SPCOMPL` | device mirror for force-only; device-owned after `SPCOMPL` is ported |
| rates and material transfer state | Fortran `WA` at its prescribed phase | named SoA arrays: `old_rho`, `strain9`, `mumax`, `divv`, `rotv`, `stress6`, `ssp`, `ssp_eq`, `sti` |
| stabilization: `STAB` | Fortran initially | device mirror for force-only; device-owned after `SPSTABW`/`SPSTABS` are ported |
| material history, failure/EOS and energy | Fortran `ELBUF_TAB` / `MMAIN` | never presented to CUDA in this migration |
| particle force/stiffness/WVIS | device after a force launch | export to `WA(7,10:12)` / `SPBUF(11)` before Fortran nodal assembly |

For a one-rank, no-symmetry context, the C++ topology can be deliberately
smaller than the legacy representation:

```text
particle_node[N]         # retained for final Fortran assembly
alive[N]                 # mirror of KXSP(2,N)>0
candidate_count[N]       # legacy KXSP(5,N), while CPU owns topology
active_count[N]          # legacy KXSP(4,N)
neighbors[KVOISPH][N]    # uint32 particle indices, neighbour-major on device
```

There is no `XSPHR`, `WACOMPR`, `ISPSYM`, `ISPSYMR`, ghost-count field, or
negative index in this initial context. The adapter validates that fact rather
than allocating unused compatibility structures.

### 10.5 Port sequence and acceptance gates

Each stage preserves the prior Fortran implementation as a fallback. Do not
combine stages: when a comparison fails, the input trace points to one phase.

| Stage | Implement | Retained Fortran work | Acceptance gate |
|---|---|---|---|
| 0 — golden traces | capture/serialize the pre-force input fields and post-force outputs for representative one-rank cycles | everything | fixtures exercise order-0/order-1 correction, viscosity, stiffness on/off, deletion candidate, and several `h` values |
| 1 — C++ CPU reference | `force_local_real()`; exact loop order and native precision | search, `SPCOMPL`, `SPDENS`, `SPSTRES`, stabilization, assembly | compare force/stiffness/WVIS per particle and aggregate force/energy to a trace |
| 2 — first CUDA kernel | direct CUDA translation of Stage 1; no CUDA nodal scatter | same as Stage 1 | repeatability with fixed neighbour order, then multi-cycle stable-model comparison |
| 3 — pre-material chain | `classify_local`, `SPCOMPL`, then `SPDENS` on device | CPU candidate search, `SPSTRES`/`MMAIN`, assembly | compare active membership/order, `WACOMP`, `rho`, strain/rates and the exact host material inputs |
| 4 — post-material chain | `SPSTABW`, `SPSTABS`, force; later `SPADAH` only after host deletion has committed activity | `SPSTRES`/`MMAIN`, deletion, assembly | compare stabilization tensor, force, time-step candidate and `h` history |
| 5 — topology | re-search criterion, GPU cell list, half-list build, explicit symmetrization, overflow policy | MPI/symmetry and material path | compare neighbour set **and order**, active/reserve split, `h` reduction and forces |
| 6 — optional device laws | selected material laws only | CPU path for all unported laws | each law has its own history-variable, failure and restart validation suite |

Stage 2 is the first actual CUDA delivery. Stages 0–1 are not optional
ceremony: a C++ CPU version separates transcription mistakes from GPU
parallel/reduction mistakes, and creates the reference needed for all later
kernels.

### 10.6 Stable C ABI and call cadence

**Recommendation:** expose an opaque C++ context through a small C ABI. A
Fortran `bind(C)` module owns conversion from legacy array layout to POD
descriptors. The CUDA library never receives a Fortran derived type, module
variable, assumed-shape descriptor or `COMMON` address.

```c
typedef struct sph_gpu_context sph_gpu_context;

int sph_gpu_create(const sph_static_desc *desc, sph_gpu_context **ctx);
int sph_gpu_upload_topology(sph_gpu_context *ctx,
                            const sph_topology_view *topology,
                            uint64_t topology_generation);
int sph_gpu_force_local(sph_gpu_context *ctx,
                        const sph_force_input_view *input,
                        sph_force_output_view *output);
int sph_gpu_pre_material(sph_gpu_context *ctx,
                         const sph_step_input_view *input,
                         sph_material_input_view *output);
int sph_gpu_upload_material_output(sph_gpu_context *ctx,
                                   const sph_material_output_view *input);
int sph_gpu_post_material(sph_gpu_context *ctx,
                          sph_result_view *output);
int sph_gpu_export_state(sph_gpu_context *ctx, sph_state_view *output);
void sph_gpu_destroy(sph_gpu_context *ctx);
```

The force-only MVP requires only `create`, `upload_topology`,
`force_local`, `export_state` and `destroy`. The pre/post-material calls are
reserved for stages 3–4:

1. **Create once** after SPH setup: validate the §10.2 feature mask; allocate
   device SoA fields and record solver precision/capacity.
2. **Upload topology** after each CPU full search while Stage 5 is not ported:
   map every positive `IXSP` node number through `NOD2SP`, reject nonlocal or
   ghost entries, and upload the active/candidate counts with a monotonically
   increasing topology generation.
3. **Per force-only cycle:** pack/upload the Stage-1 inputs; run the local
   force kernel; copy only force, stiffness and WVIS back; write their legacy
   slots; then use the existing Fortran nodal assembly.
4. **After Stage 3:** run device classification/correction/density; copy the
   precise `SPLOAD3` material inputs to the host; execute unchanged
   `SPSTRES`/`MMAIN`; upload the `SPBACK3` outputs; then run device
   stabilization/force.
5. **Export before a host consumer:** synchronize state before restart/output,
   CPU fallback, changing device ownership, or returning to an unported phase.

Every ABI routine returns a status. The adapter must surface an error through
the engine's normal error path and recompute the uncommitted phase on the CPU;
it must not leave half-written `WA`, `SPBUF`, or `A` values.

### 10.7 Transfer and topology rules

The force-only milestone is intentionally transfer-heavy because all its inputs
are still produced by Fortran. It is a correctness and integration milestone,
not a promise of end-to-end acceleration.

| Migration stage | H→D each cycle | D→H each cycle | Kept resident |
|---|---|---|---|
| force-only (Stage 2) | local kinematics; `h/rho/mass`; stress/sound-speed/rate fields; `WACOMP`; `STAB`; property constants when not static | force3, stiffness, WVIS | converted topology until the next CPU re-search |
| pre/post-material GPU (Stages 3–4) | current kinematics, host-mutated activity/topology state; `SPBACK3` material results | exact `SPLOAD3` material inputs; force3/stiffness/WVIS; host-required `h/rho` | topology, corrections, rates, stabilization and force scratch |
| GPU topology (Stage 5) | current particle kinematics/state and topology-generation controls | only legacy consumers, output/restart snapshots and force results | all local SPH topology and derived state |

The following events invalidate the device topology or require full state
export: a CPU neighbour rebuild, particle deletion (`KXSP(2,N)` changes),
solid-to-SPH conversion, inlet/outlet activity, symmetry/MPI activation,
nodal-to-particle remapping, restart/output that consumes device-owned data,
or a CUDA error. Before committing device results, retain sufficient host
pre-phase state to recompute that phase with the original Fortran code.

### 10.8 Numerical and release rules

The C++ CPU reference and first CUDA kernel should reproduce the existing
arithmetic order. Do **not** begin by exploiting the symmetric `L` matrix,
changing pair storage, replacing the correction scheme, or making forces
pairwise antisymmetric. Those are separate, physics-affecting improvements
(§5.2, §9.9), not porting prerequisites.

For the requested FP32 target, first demonstrate a native-precision CUDA path,
then gate FP32 behind separate checks for:

- correction-matrix conditioning and the existing unguarded determinant
  inverse (`spcompl.F:404-425`);
- FP32-safe guard constants and kernel normalization (§9.6);
- the FP64 stabilization eigensolve requirement (`spstab.F:228-229`);
- fixed-order reductions, time-step conservatism and restart policy;
- force/density/energy tolerances agreed per reference deck.

The first release should therefore be opt-in and all-or-nothing for a run:
eligible deck → CUDA path; unsupported feature or runtime error → original
Fortran SPH path. Do not select GPU/CPU piecemeal per particle; that would
destroy the consistent topology, correction and force-state assumptions.

---

## 11. Reference index

### Entry points
| What | Where |
|---|---|
| SPH prep (sort, symmetrize, `WACOMP`) | `engine/source/engine/resol.F:4324` → `sphprep.F` |
| SPH physics | `engine/source/engine/resol.F:4370` → `forintp.F` |
| XSPH velocity smoothing | `engine/source/engine/resol.F:6821` → `splissv.F` |

### Key line references
| Topic | Reference |
|---|---|
| Cubic B-spline kernel | `weight.F:53-65` (`WEIGHT0`), `:98-114` (`WEIGHT1`) |
| Pair smoothing length `h_ij` | `spforcp.F:177`, `spdens.F:149`, `spcompl.F:322` |
| `alpha_i` and `∇alpha_i` | `spcompl.F:247-256` |
| Moment matrix `L_i` | `spcompl.F:328-336` |
| `L_i⁻¹` (negated) | `spcompl.F:404-425` |
| Corrected gradient (particle `i`) | `spforcp.F:184-192`, `spdens.F:169-192` |
| Conjugate kernel (particle `j`) | `spforcp.F:223-228` |
| Strain rate | `spdens.F:199-235` |
| `DIVV`, `ROTV` | `spdens.F:250-252, 378-386` |
| Continuity density | `spdens.F:378-381` |
| Volume from density | `spvol3.F:72-74` |
| Artificial viscosity | `spforcp.F:286-305` |
| Stress force | `spforcp.F:241-258` |
| Stabilization force | `spforcp.F:262-277` |
| Force accumulation (gather-only) | `spforcp.F:515-520, 709-711, 897-899` |
| Nodal stiffness | `spforcp.F:307-334` |
| Nodal assembly | `forintp.F:861-909` |
| Voxel search box | `sptrivox.F:274-292` |
| Half-list ordering filter | `sptrivox.F:313-314` |
| Distance acceptance | `sptrivox.F:333-356` |
| Explicit symmetrization | `spbuc3.F:169-317` |
| `KVOISPH` truncation | `spbuc3.F:364-427` |
| Active/reserve split | `spbuc3.F:430-444`, `spclasv.F:290-333` |
| Adaptive `h` reduction | `spclasv.F:104-199` |
| Parith/ON neighbour ordering | `spclasv.F:336-345` |
| Re-search criterion | `sphprep.F:365-378`, `:396-420` |
| Search reference state | `sphtri.F:92-95` |
| Stabilization weight | `spstab.F:122-131` |
| Stabilization tensor | `spstab.F:219-259` |
| Material time step | `engine/source/materials/mat_share/mdtsph.F:127-166` |
| Nodal SPH time step / deletion | `forintp.F:1241-1278` |
| Variable `h` | `spadah.F:78-98` |
| Constitutive driver | `spstres.F:199-322` |
| Gather / scatter | `spload3.F:31-72`, `spback3.F:58-73` |
| SPH law compatibility | `starter/.../check_mat_elem_prop_compatibility.F:72-90, 154-166`, `init_mat_keyword.F:274-275` |
| `KXSP` authoritative map | `starter/source/elements/reader/hm_read_sphcel.F:69-77` |
| `XSPHR` layout | `engine/share/modules/sphbox.F` |
| Global SPH constants | `engine/share/includes/sphcom.inc` |
| `/SPHGLO` defaults | `starter/.../hm_read_sphglo.F:77-107` |
| `/PROP/TYPE34` (`Xi_Stab`) | `starter/source/properties/sph/hm_read_prop34.F:161-172` |

### Related documents
- `doc/ENGINE_TIME_LOOP_documentation.md` — where `FORINT`/`FORINTP` sit in the cycle
- `doc/GROUPS_AND_CONNECTIVITY_documentation.md` — `IPARG`, `MVSIZ`, nodal arrays
- `doc/ELBUF_TAB_documentation.md` — `GBUF`/`LBUF` element state
- `doc/SPMD_documentation.md` — MPI decomposition and Parith ON/OFF
