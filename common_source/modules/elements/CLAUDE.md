# modules/elements/

## Purpose
General element data structures and the SFEM (Solid Finite Element Method) module for tetrahedron element computations.

## Files

| File | Module | Description |
|------|--------|-------------|
| `element_mod.F90` | `ELEMENT_MOD` | General element data structure definitions; extensively referenced by element processing routines throughout the engine |
| `sfem_mod.F90` | `SFEM_MOD` | Types `spmd_` (MPI communication pattern) and `sfem_` (SFEM computation buffers: tetra element data, nodal arrays, connectivity) |

## Key Types Exported
- **`sfem_`** — SFEM element computation state (tetra buffers, nodal arrays)
- **`spmd_`** — MPI frontier communication structure for SFEM

## Dependencies
- Uses: `modules/precision_mod.F90`, `modules/mat_elem/elbufdef_mod.F90`
- Used by: engine solid element routines and SFEM solver
