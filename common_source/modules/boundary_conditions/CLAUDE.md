# modules/boundary_conditions/

## Purpose
Eulerian and general boundary condition data structure definitions used by ALE/FVM solvers: face data, non-reflecting (NRF) boundaries, and wall boundaries.

## Files

| File | Module | Description |
|------|--------|-------------|
| `ebcs_mod.F90` | `EBCS_MOD` | Eulerian BC types: `bcs_face_data_` (face element data), `bcs_nrf_struct_` (non-reflecting boundary), `bcs_wall_struct_` (wall boundary parameters) |
| `bcs_mod.F90` | `BCS_MOD` | General BC types: `bcs_face_data_` and `bcs_wall_struct_` for non-Eulerian boundary conditions |

## Key Types Exported
- **`bcs_face_data_`** — face element boundary data
- **`bcs_nrf_struct_`** — non-reflecting (NRF) boundary condition data
- **`bcs_wall_struct_`** — wall boundary condition parameters

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: ALE/FVM boundary condition processing routines in the engine
