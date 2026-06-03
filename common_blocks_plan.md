# Plan: Document Common Blocks in OpenRadioss for UDT Refactoring

## Problem Statement

OpenRadioss uses Fortran `COMMON` blocks (defined in `scr*.inc` and `com*.inc` files) as a
global state mechanism. These were originally organised by purpose but have grown organically
and now contain a mixture of concerns. The goal is to document every variable in every common
block to understand what logical groups they belong to, as a prerequisite for replacing them
with user-defined types (UDTs) passed as arguments.

**This plan covers documentation only — no code changes.**

---

## Scope

### Files to analyse

#### Engine (`engine/share/includes/` and `engine/share/spe_inc/`)
| File | Common Block(s) | # Including files |
|---|---|---|
| `scr01_c.inc` | `SCR01` (task_common) | 42 |
| `scr02_c.inc` | `SCR02` | 387 |
| `scr03_c.inc` | `SCR03` | 958 |
| `scr04_c.inc` | `SCR04` (task_common) | 17 |
| `scr05_c.inc` | `SCR05` | 927 |
| `scr06_c.inc` | `SCR06`, `SCR06R` | 376 |
| `scr07_c.inc` | `SCR07` | 643 |
| `scr08_a_c.inc` | `SCR08_A` | 45 |
| `scr11_c.inc` | `SCR11` | 270 |
| `scr12_c.inc` | `SCR12` | 206 |
| `scr13_c.inc` | `SCR13` | 89 |
| `scr14_c.inc` | `SCR14`, `ANIVAR1-6` | 1180 |
| `scr16_c.inc` | `SCR16`, `SCR16R`, `SCR16_STATR`, `SCR16_STATI` | 956 |
| `scr17_c.inc` | `SCR17` | 3180 |
| `scr18_c.inc` | `SCR18`, `SCR18R` | 1087 |
| `scr19_c.inc` | `SCR19`, `SCR19R` | 126 |
| `scr20_c.inc` | `SCR20` | 35 |
| `scr23_c.inc` | `SCR23` | 262 |
| `scr25_c.inc` | `SCR25`, `ANIVAR1-6` | 78 |
| `scr_fac_c.inc` | `SCRFCI`, `SCRFCR` | 47 |
| `scrcut_c.inc` | `SCRCUT` | 102 |
| `scrfs_c.inc` | `SCRFS` | 83 |
| `scrnoi_c.inc` | `SCRNO_I`, `SCRNO_R` | 95 |
| `com01_c.inc` | `COM01` | 8224 |
| `com04_c.inc` | `COM04` | 10247 |
| `com06_c.inc` | `COM06` | 860 |
| `com08_c.inc` | `COM08`, `COM08DP` | 3964 |
| `com09_c.inc` | `COM09` | 152 |
| `com10_c.inc` | `COM10` | 116 |
| `com20_c.inc` | `COM20` | 71 |
| `com_xfem1.inc` | `XFEMI` | 667 |
| `comlock.inc` | `LOCKMP` | 4080 |
| `commandline.inc` | `CMDLINE` | (both) |

#### Starter (`starter/share/includes/`)
The starter has overlapping files but also some unique ones:
- `scr01_c.inc` — different content (NB1..NB11 memory block counters, not SCR01/ITASKP1)
- `scr10_c.inc` — `SCRALE1` (ALE BCS count)
- `scr15_c.inc` — `SCR15` (root file name), `SCR15I`
- `scr21_c.inc` — `SCR21` (IDEFAULT)
- `scr22_c.inc` — `SCR22` (INER_9_12)
- `scry_c.inc` — `SCRY` (element counts by type)
- `com_engcards_c.inc` — `COM_ENGCARDS` (engine run parameters read from starter)

---

## Approach

The documentation task decomposes into three phases:

### Phase 1 — Inventory (per-file variable extraction)
For each `.inc` file:
- Extract: common block name, all variable names, their Fortran types
- Record array dimensions where present
- Note whether the file is engine-only, starter-only, or shared (with differences)

### Phase 2 — Variable Annotation (per-variable purpose)
For each variable in each common block:
- **Name analysis**: Fortran naming conventions encode meaning (N*=count, I*=integer flag,
  D*/DT*=time delta, T*=time/table, L*=logical/length, S*/NS*=selected counts, etc.)
- **Usage search**: `grep` for variable name across the codebase to find:
  - where it is set (initialisation routines)
  - where it is consumed (physics/output/control routines)
  - what module/subdirectory uses it most
- **Annotation**: record inferred purpose and usage domain

### Phase 3 — Grouping Proposal (UDT candidates)
Based on Phase 2, group variables into candidate UDTs with logical names. Expected groups:

| Candidate UDT | Likely contents | Source commons |
|---|---|---|
| `simulation_control_t` | flags: N2D, IALE, IEULER, ILAG, ICRACK, IRIGID_MAT, ... | COM01 (large part) |
| `cycle_counters_t` | NCYCLE, IRUN, NSPMD, NTHREAD0, ... | COM01 |
| `entity_counts_t` | NUMMAT, NUMNOD, NUMELQ, NUMELS, NUMELC, NUMELT, NINTER, ... | COM04 |
| `time_control_t` | TT, DT1, DT2, TSTOP, DTFAC, DTMIN, TOUTP, ... | COM06, COM08 |
| `output_control_t` | NCPRI, IANIM, IOUTP, NHIN2, NV_ANI, NT_ANI, DTOUTP, ... | COM09, SCR14, SCR16 |
| `timestep_data_t` | DTFAC1, DTMIN1, IDTMIN, IDTGR, PERCENT_ADDMASS, ... | SCR18, SCR18R |
| `element_scratch_t` | NELTS, ITYPTS, NODADT, ... | SCR02, SCR03 |
| `deletion_control_t` | IDEL7, IDT1SH, IDEL7NG, IDEL7NOK, ... | SCR17 |
| `energy_balance_t` | ENINT, ENCIN, XMOMT, YMOMT, ZMOMT, XMASS, ENROT | SCR11 |
| `animation_vars_t` | ANIM_EPSP, ANIM_STRESS, ANIM_DAMA, ANIM_STRAIN, ... | SCR25, ANIVAR1-6 |
| `xfem_control_t` | NUMELCRK, ICRACK3D, NXEL, NCRKPART, ... | XFEMI |
| `parallel_lock_t` | LLOCK | LOCKMP |
| `commandline_opts_t` | GOT_NCPU, NCPU, NTH, GOT_TIMER, ... | CMDLINE |
| `numerical_quadrature_t` | Z0, WF, WM, ZTH | COM20 |
| `noise_control_t` | NNOISE, TNOISE, DTNOISE, NOISEV, NOISEP, NOISEA | SCRNOI |
| `file_io_t` | IUNIT, IFREEF, MDESS, MREST, MSTOP, MANIM | SCR13, SCR07 |

These are initial hypotheses — the actual groupings will emerge from Phase 2.

---

## Methodology Details

### Variable search strategy
For a variable like `NCYCLE` in `COM01`:
```sh
grep -rn "\bNCYCLE\b" engine/source starter/source --include="*.F" --include="*.F90" | head -30
```
Look for patterns:
- Set once at init → likely a "run configuration" variable
- Updated every cycle → likely a "solver state" variable
- Passed as argument already in some places → candidate for early extraction

### ⚠️ Thread-Private Variables — Special Handling Required

A grep for `!$OMP THREADPRIVATE` and `thread_private` across **all** `*.inc` files reveals
the following blocks that require special treatment during any UDT refactoring:

| Block | File | Scope | Key variables | Risk level |
|---|---|---|---|---|
| `SCR04` | `engine/share/includes/scr04_c.inc` | Engine | `NRTM, NRTS, NMN, NSN, NTY, NST, MST, NLINM, NLINS, NLINMA, NLINSA, NMNE, NSNE` | Medium — contact/link counters, 17 use-sites |
| `VECT01` | `engine/share/includes/vect01_c.inc` | Engine | `LFT, LLT, NFT, MTN, IAD, ITY, NPT, JALE, ISMSTR, ...` (36 vars) | **CRITICAL** — element loop bounds used by all force routines |
| `UPLAS` | `engine/share/includes/usrplas_c.inc` | Engine | `U_YELD(MVSIZ), U_ETSE(MVSIZ), U_DEFP(MVSIZ)` | High — user material yield/modulus/plastic-strain scratch |
| `UTAG` | `engine/share/includes/usrplas_c.inc` | Engine | `U_TAGPLAS(MVSIZ)` | High — user material plasticity tag |
| `UVAR` | `engine/share/includes/usrplas_c.inc` | Engine | `UUVAR(MVSIZ,5000)` | High — very large user-variable scratch per thread |
| `VEC_SPRING_NUM` | `engine/share/includes/vec_spring_num.inc` | Engine | `SPR_NUM(MVSIZ)` | Medium — spring element scratch |
| `IMPL1_PRIVATE` | `engine/share/includes/impl1_c.inc` | Engine | `NG_IMP` | Medium — implicit solver per-thread group counter |
| `UNITS_2` | `starter/share/includes/units_fxbody_c.inc` | Starter | `IFXM_L, IFXS_L` | Medium — fixed-body unit numbers; comment in file confirms threading requirement |

**How `task_common` works**: The macro in `engine/share/spe_inc/task_common.inc` expands to
plain `COMMON` under OpenMP (`#define task_common COMMON`). Thread-privacy under OpenMP is
**only** guaranteed by an explicit `!$OMP THREADPRIVATE` directive immediately following the
`COMMON` declaration. Blocks using `task_common` without that directive are **not**
thread-private at runtime.

**Refactoring constraint**: These blocks must not be converted to a single shared UDT variable.
Options:
- Declare the replacement UDT variable with `!$OMP THREADPRIVATE(var)` (minimal change, keeps global state)
- Pass as a dummy `INTENT(INOUT)` argument (preferred: eliminates global state, makes threading explicit)
- Use a thread-indexed array `type_t :: arr(0:nthreads-1)` indexed by `omp_get_thread_num()`

**Migration order recommendation**: Migrate `VECT01` last. It is included in virtually
every element force routine; converting it before the rest of the call tree is clean would
create an inconsistent intermediate state.

### Divergence analysis (engine vs starter)
For shared files (same name, different content), compare variable sets to understand
which variables are "shared concept, different context" vs. "different concept, same slot".
The starter version of COM04 and COM01 are the most significant divergences.

### Output artefact
Produce a Markdown document (saved to session files/) containing:
- One section per `.inc` file
- Table: variable name | type | array dims | inferred purpose | usage domain | proposed UDT
- A summary table mapping old common block → proposed UDT(s)
- Notes on variables that are hard to classify (candidates for further discussion)

---

## Todos

| ID | Title | Description |
|---|---|---|
| `inventory-engine` | Inventory engine common blocks | Extract and tabulate all variables from engine scr/com .inc files |
| `inventory-starter` | Inventory starter common blocks | Extract and tabulate all variables from starter scr/com .inc files, noting divergences from engine |
| `annotate-com01` | Annotate COM01 variables | Search and classify all variables in COM01 (most widely used: 8224 files) |
| `annotate-com04` | Annotate COM04 variables | Search and classify all variables in COM04 (most widely used: 10247 files) |
| `annotate-com08` | Annotate COM08 variables | Time/DT variables |
| `annotate-com06` | Annotate COM06 variables | Time step control real variables |
| `annotate-scr17` | Annotate SCR17 variables | Deletion control (3180 files) |
| `annotate-scr14` | Annotate SCR14 variables | Animation variable lists |
| `annotate-scr16` | Annotate SCR16 variables | Output control |
| `annotate-scr18` | Annotate SCR18 variables | DT per element group |
| `annotate-remaining-scr` | Annotate remaining SCR blocks | scr02,03,05,06,07,08,11,12,13,19,20,23,25,scrcut,scrfs,scrnoi,scr_fac |
| `annotate-remaining-com` | Annotate remaining COM blocks | com09,com10,com20,xfem1,comlock,cmdline |
| `annotate-starter-only` | Annotate starter-only blocks | scr10,scr15,scr21,scr22,scry,com_engcards |
| `propose-udts` | Propose UDT groupings | Synthesise annotations into candidate UDT definitions with rationale |
| `produce-doc` | Produce documentation output | Write final Markdown document to session files/ |
