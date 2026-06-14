# engine/source/ale/

## Purpose
Arbitrary Lagrangian-Eulerian (ALE) and pure Eulerian advection: fluid transport,
mesh rezoning, multi-material mixing, ALE grid motion, and thermal advection.
Called once per cycle from `RESOL` via `ALEMAIN` before element force loops.

## Main entry point

`ALEMAIN` (`alemain.F`) is the single dispatcher called from `RESOL` (line ~3465).
It decides which ALE sub-model is active and calls the appropriate sub-routines.

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `ale2d/` | 2D ALE advection (cell-centred) |
| `ale3d/` | 3D ALE advection |
| `ale51/` | ALE TYPE 51 (fluids with arbitrary EOS) |
| `alefvm/` | ALE Finite-Volume Method: cell flux, donor-cell, MUSCL scheme |
| `alemuscl/` | MUSCL slope limiter for ALE advection |
| `bimat/` | Bi-material interface tracking |
| `euler2d/` | 2D Eulerian (pure) advection |
| `euler3d/` | 3D Eulerian advection |
| `grid/` | ALE grid velocity/rezoning: `AREZON`, `ACONVE`, flow-tracking exchange |
| `inter/` | ALE interface coupling to Lagrangian boundaries |
| `porous/` | Porous ALE media |
| `subcycling/` | ALE subcycling (shorter ALE time step inside Lagrangian cycle) |
| `turbulence/` | K-ε turbulence model for ALE/Euler flows |

## Key files in root `ale/`

| File | Role |
|------|------|
| `alemain.F` | ALE cycle dispatcher |
| `aconve.F90` | ALE advection convection step |
| `arezon.F90` | ALE mesh re-centering (rezoning) |
| `init_ale.F90` | ALE initialization from restart data |
| `agauge.F`, `agaug3q.F`, `agaug3t.F` | ALE gauge/sensor data extraction |
| `agrad0.F` | ALE gradient computation |
| `alethe.F`, `atherm.F` | ALE thermal advection |
| `aflux0.F` | ALE flux computation |

## Dependencies
- Called by: `RESOL` → `ALEMAIN`
- MPI exchange: `engine/source/mpi/ale/` (min/max exchange, neighbor exchange, cell exchange)
- Uses: `multimat_param_mod.F90` for multi-material ALE element state
- ALE elements are also part of `engine/source/elements/solid/` group kernels with `JALE` flag set
