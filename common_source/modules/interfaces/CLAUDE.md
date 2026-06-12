# modules/interfaces/

## Purpose
Contact/interface Fortran module definitions: master interface data structures, per-type buffer definitions, friction model parameters, cut-cell data for type-22 interfaces, and MPI communication arrays.

## Files

| File | Module | Description |
|------|--------|-------------|
| `interfaces_mod.F90` | `INTERFACES_MOD` | Master interface data structures; most widely used interface module — defines the top-level interface entity type |
| `intbufdef_mod.F90` | `INTBUFDEF_MOD` | Interface buffer type definitions (`INTBUF_STRUCT_` and related); the primary buffer type used by `interf/` utilities |
| `int8_mod.F90` | `INT8_MOD` | Type-8 (3D penalty contact): `buft8` (main/secondary node buffer management) and `front8` (MPI communication pattern) |
| `intbuf_fric_mod.F90` | `INTBUF_FRIC_MOD` | Friction model parameter type `intbuf_fric_struct_` |
| `cut-cell-buffer_mod.F` | `I22EDGE_MOD` | Cut-cell buffer for type-22 (brick/surface intersection): edge entity and cut-plane structures for embedded mesh intersections |
| `cut-cell-search_mod.F` | `I22BUFBRIC_MOD` | Cut-cell search structures and buffer management for type-22 interface search operations |
| `spmd_arrays_mod.F` | `SPMD_ARRAYS_MOD` | Type `SPMD_ARRAYS_` with frontier edge arrays for type-25 contact MPI communication |
| `metric_mod.F` | `METRIC_MOD` | Type `METRIC_STRUCT_` for interface sorting algorithm selection and performance tracking |
| `th_surf_mod.F` | `TH_SURF_MOD` | Type `TH_SURF_` for `/TH/SURF` time-history output (pressure, area, load flags) |
| `parameters_mod.F` | `PARAMETERS_MOD` | Type `PARAMETERS_` containing stiffness computation flags and nodal area arrays |

## Key Types Exported
- **`INTBUF_STRUCT_`** — primary interface buffer (used by `interf/intbuf_ini.F`)
- **`INTERFACES_MOD`** — master interface entity type
- **`INT8_MOD`** — type-8 contact buffers

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: engine interface processing (`source/interfaces/`), `interf/` utilities
