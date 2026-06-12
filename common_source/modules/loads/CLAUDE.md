# modules/loads/

## Purpose
Load type definitions and domain decomposition data for distributing loads across MPI processors: pressure loads (cylindrical, blast), general load aggregation, and domain decomposition.

## Files

| File | Module | Description |
|------|--------|-------------|
| `loads_mod.F90` | `LOADS_MOD` | Aggregate type `loads_` holding load counts and parameters; depends on `pload_cyl_mod`, `domdec_load_mod`, `inivel_mod` |
| `pload_cyl_mod.F` | `PLOAD_CYL_MOD` | Type `PRESS_CYL_` for cylindrical pressure loads: ID, reference frame, sensor, function table, segments, domain decomposition data |
| `domdec_load_mod.F` | `DOMDEC_LOAD_MOD` | Types `DOMDEC_PROC_LOAD_` and `DOMDEC_LOAD_` for distributing load contributions across MPI processors |
| `pblast_mod.F90` | `PBLAST_MOD` | Type `pblast_struct_` for blast (Friedlander curve) pressure loads: parameters, angle, reset flags |

## Key Types Exported
- **`loads_`** — aggregate load container
- **`PRESS_CYL_`** — cylindrical pressure load
- **`pblast_struct_`** — blast load parameters
- **`DOMDEC_LOAD_`** / **`DOMDEC_PROC_LOAD_`** — MPI load distribution

## Dependencies
- Uses: `modules/precision_mod.F90`, `modules/inivel_mod.F90`
- Used by: engine load application routines
