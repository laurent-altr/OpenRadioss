# engine/source/fluid/

## Purpose
Compressible fluid solver (CFD) for pure Eulerian elements: Euler/Navier-Stokes
equations, boundary element method (BEM) for acoustic/fluid coupling, and
incompressible-pressure flow. Used for `/ALE/` and `/EULER/` models.

## Key files

| File | Role |
|------|------|
| `flow0.F` | Main fluid flow dispatcher: initializes and calls the cell-centred fluid solver |
| `flow1.F` | Fluid time-step integration: cell update |
| `fluxsw.F` | Flux switching between donor-cell and MUSCL schemes |
| `incpflow.F` | Incompressible pressure flow solver |
| `lecflsw.F` | Reads flow-switch control parameters |
| `freupwm.F` (in `input/`) | Reads upwind scheme parameters |
| `bemsolv.F` | BEM (Boundary Element Method) solver — acoustic/fluid surface coupling |
| `bemsolvp.F` | BEM parallel variant |
| `daaacc.F` | Double-precision acoustic acceleration assembly |
| `daasolv.F` | Acoustic solver (modal/direct) |
| `daasolvp.F` | Acoustic solver parallel variant |
| `nintrn.F` | Fluid-structure interface normal computation |

## Relation to ALE
The `fluid/` routines handle **pure Eulerian** (fixed-grid) elements. ALE
advection (moving mesh) is in `engine/source/ale/`. The two share the
multi-material parameter module (`multimat_param_mod.F90`).

MPI exchange for CFD: `engine/source/mpi/fluid/spmd_cfd.F` — contains
`SPMD_E1VOIS`, `SPMD_E4VOIS`, `SPMD_EXALEW`, `SPMD_XVOIS`, `SPMD_EXTAG`, etc.

## Dependencies
- Called by: `RESOL` via the ALE/Euler section (when `IALE > 0` or `IEULER > 0`)
- Uses: `multimat_param_mod.F90`, `engine/source/mpi/fluid/`
