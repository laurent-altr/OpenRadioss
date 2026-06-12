# engine/source/elements/

## Purpose
All element type implementations: force computation, hourglass control, time-step
contribution, coordinate update, and element-level initialization. The main
dispatchers (`FORINT`, `FORINTC`, `FORINTS`, `FORINTP`) are called from `RESOL`
each cycle; they loop over element groups and call the appropriate element kernel.

## Root-level dispatchers

| File | Role |
|------|------|
| `forint.F` | **Solid + other element force loop**: loops NG=1..NGROUP; dispatches by `IPARG(5,NG)` to solid, beam, spring, 3-node shell, thick shell, user element kernels |
| `forintc.F` | **4-node shell force loop** (coque4n family): called separately from `FORINT` |
| `forints.F` | **SPH / 1D element force loop** |
| `forintp.F` | **SPH particle force loop** |
| `desacti.F` | Element deactivation / erosion: marks elements as failed (`GBUF%off = 1`) |
| `eloff.F` | Element off-flag utility (checks/sets element deletion) |
| `findgroup.F` | Finds the group index NG for a given element ID |

## Sub-directories

### `solid/` — Solid elements (3D continuum)

| Sub-dir | Element type | Entry point |
|---------|-------------|-------------|
| `solide/` | 8-noded hexahedral (default, reduced integration + hourglass control) | `SFORC3` |
| `solide4/` | 4-noded tetrahedron (constant stress, simplest) | `S4FORC3` |
| `solide4_sfem/` | 4-noded tetrahedron SFEM (Smoothed FEM, more accurate) | `S4LAGSFEM`, `S4ALESFEM` |
| `solide8/` | 8-noded hex, full integration (2×2×2 Gauss) | `S8FORC3` |
| `solide8e/` | 8-noded hex, enhanced strain | `S8EFORC3` |
| `solide8s/` | 8-noded hex, sub-integrated | `S8SFORC3` |
| `solide8z/` | 8-noded hex + EOS (hydrodynamic) | `S8ZFORC3` |
| `solide10/` | 10-noded tetrahedron (quadratic) | `S10FORC3` |
| `solide20/` | 20-noded hexahedron (quadratic) | `S20FORC3` |
| `solide6z/` | 6-noded pentahedron (wedge) + EOS | `S6ZFORC3` |
| `solidez/` | Generic solid + EOS (alternative) | `SZFORC3` |
| `sconnect/` | Solid connection elements (connector/fastener) | |

Key files in `solide/`:
- `sforc3.F` — main solid force (reduced integration + Flanagan-Belytschko hourglass)
- `scoor3.F` — coordinate update
- `sbilan.F` — energy balance
- `sgcoor3.F` — geometric (large deformation) coordinate update

### `shell/` — 4-node quadrilateral shells

| Sub-dir | Formulation | Entry point |
|---------|-------------|-------------|
| `coque/` | Belytschko-Lin-Tsay (BLT) reduced-integration shell | `CFORC3` |
| `coqueba/` | Batoz-Dhatt (BDT) shell variant | `CBAFORC3` |
| `coquez/` | BLT shell + EOS (for ALE/fluid-shell) | `CZFORC3` |

Key files in `coque/`:
- `cforc3.F` — main shell force (in-plane + bending, BLT formulation)
- `ccoor3.F` — shell coordinate update (nodal positions, normal)
- `cbilan.F` — energy balance
- `coqhourg.F` / `coqhourg1.F` — hourglass control
- `coqthk.F` — shell thickness update

The `coque/` kernels are called by `FORINTC` (separate from `FORINT`).

### `thickshell/` — Thick shell (3D shell with through-thickness integration)

| Sub-dir | Description | Entry point |
|---------|-------------|-------------|
| `solidec/` | Main thick shell (8-node with multiple through-thickness layers) | `SCFORC3` |
| `solide8c/` | 8-noded thick shell variant | `S8CFORC3` |
| `solide6c/` | 6-noded wedge thick shell | `S6CFORC3` |
| `solide16/` | 16-noded quadratic thick shell | `S16FORC3` |

### `sh3n/` — 3-node triangular shells

3-node shells are dispatched from `FORINT` (not `FORINTC`):
- `coque3n/` — constant stress triangle (CST)
- `coque3ba/` — DKT (Discrete Kirchhoff Triangle) variant

### `beam/` — Beam elements

| File | Role |
|------|------|
| `main_beam3.F` | Main beam force dispatcher |
| `main_beam18.F` | TYPE 18 beam (thin-walled section) |
| `fail_beam3.F`, `fail_beam18.F` | Beam failure |
| `pbilan.F` | Beam energy balance |
| `pcoor3.F`, `pcoork3.F` | Beam coordinate update |
| `pcurv3.F` | Beam curvature |
| `pdamp3.F` | Beam damping |

### `spring/` — Spring/damper elements

| File | Role |
|------|------|
| `rforc3.F` (in `spring/`) | Main spring force: computes spring force from relative displacement/velocity, applies nonlinear law |
| `preload_axial.F90` | Spring preload |
| `r12ke3.F`, `r13ke3.F` | Spring stiffness (TYPE 12/13) |
| `r12mat3.F`, `r13mat3.F` | Spring material law |
| `r1coor3.F`, `r1coork3.F` | Spring coordinate update |
| `r1cum3.F` | Spring cumulative deformation |

### `truss/` — Truss (2-node bar) elements

| File | Role |
|------|------|
| `tforc3.F` | Truss force (axial force only) |
| `tcoor3.F`, `tcoork3.F` | Coordinate update |
| `tdefo3.F`, `tdlen3.F` | Deformation / length |
| `tbilan.F` | Energy balance |
| `tfcum3.F`, `tfcum3p.F` | Force cumulation |
| `tke3.F`, `tkeg3.F` | Stiffness |

### `sph/` — SPH (Smoothed Particle Hydrodynamics)

Large sub-area with neighbor search, kernel evaluation, and force computation.
Dispatched from `FORINTS` and `FORINTP`. Key files:
- `sphprep.F` — SPH neighbor search and kernel weight computation
- `sphtri0.F` — SPH force computation loop
- `sph_crit_voxel.F90` — voxel-based neighbor search

### `joint/` — Joint elements

| File | Role |
|------|------|
| `rgjoint.F` | Main joint force: rigid joint kinematic constraint |
| `joint_block_stiffness.F` | Joint stiffness matrix |
| `joint_elem_timestep.F` | Joint time-step |
| `ranim33.F`, `rbilan33.F` | Joint animation / energy |
| `rcum33.F`, `rcum33p.F` | Joint cumulative deformation |
| `rdtime33.F` | Joint DT |
| `rskew33.F` | Joint local frame |
| `ruser33.F` | User-defined joint |

### `rivet/` — Rivet/spot-weld elements
Specialized connector elements for sheet-metal joining.

### `xfem/` — XFEM (eXtended FEM) crack elements
Used with `/PROP/XFEM` to model crack propagation through solid elements:
- `cforc3_crk.F`, `sforc3_crk.F` — cracked element force
- `accele_crk.F`, `asspar_crk.F` — cracked node acceleration/assembly
- `activ_xfem.F` — crack activation trigger

### `xelem/` — User-defined element interface

### `ige3d/` — IG3D (Integrated Green 3D) elements

### `elbuf/` — Element buffer initialization
- `elbuf_ini.F` (cross-referenced from `starter/`; engine-side allocation helpers)

### `solid_2d/` — 2D solid elements (plane stress/strain, axisymmetric)

## Dispatch table (`FORINT`, `forint.F`)

The `SELECT CASE (IPARG(5,NG))` in `FORINT` maps element type values to kernels:

| `IPARG(5,NG)` | Element | Kernel |
|---------------|---------|--------|
| 1 | 8-noded hex (default) | `SFORC3` |
| 2 | 8-noded hex + EOS | `S8ZFORC3` |
| 3 | 4-noded tet | `S4FORC3` |
| 4 | 8-noded hex full integration | `S8FORC3` |
| 5 | 4-noded tet SFEM | `S4LAGSFEM` |
| 6 | 6-noded wedge | `S6ZFORC3` |
| 8 | 3-node triangle shell | `C3FORC3` |
| 14 | 20-noded hex | `S20FORC3` |
| 17 | Thick shell | `SCFORC3` |
| 20 | Beam | `MAIN_BEAM3` |
| 21 | Truss | `TFORC3` |
| 22 | Spring | `RFORC3` |
| 23 | Joint | `RGJOINT` |
| 30 | 10-noded tet | `S10FORC3` |
| 42 | XFEM cracked solid | `SFORC3` + XFEM overlay |
| ... | (many more; see `forint.F` for complete list) | |

## Dependencies
- Dispatchers `FORINT`/`FORINTC` called by: `RESOL` (Steps 5–6)
- Element kernels call: `MMAIN8` / `MULAWC` (material), `MEOS8` (EOS), `FAIL_*` (failure)
- MPI exchange: `engine/source/mpi/elements/` (secondary force exchange)
- See `doc/ELBUF_TAB_documentation.md` for how element state is stored
- See `doc/GROUPS_AND_CONNECTIVITY_documentation.md` for `IPARG` and connectivity
