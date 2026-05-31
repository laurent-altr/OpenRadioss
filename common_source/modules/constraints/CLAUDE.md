# modules/constraints/

## Purpose
Constraint element type definitions: RBE3 rigid body interpolation elements and rigid wall constraints, including MPI communication structures and penalty formulation data.

## Files

| File | Module | Description |
|------|--------|-------------|
| `rbe3_mod.F90` | `RBE3_MOD` | Types `rbe3_mpi` (MPI communication pattern), `rbe3_pen` (penalty formulation parameters), `rbe3_` (master RBE3 entity: node lists, buffers, penalty data) |
| `rwall_mod.F90` | `RWALL_MOD` | Types `rwall_pen` (penalty parameters) and `rwall_` (rigid wall: nodal lists, force/moment arrays, buffers) |

## Key Types Exported
- **`rbe3_`** / **`rbe3_pen`** / **`rbe3_mpi`** — RBE3 constraint element with penalty and MPI data
- **`rwall_`** / **`rwall_pen`** — rigid wall with penalty parameters and force/moment arrays

## Dependencies
- Uses: `modules/precision_mod.F90`, `linearalgebra/` (for solver)
- Used by: engine constraint assembly and enforcement routines
