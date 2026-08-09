# Load Modules (`common_source/modules/loads/`)

Fortran 90 modules defining data types for loads. Shared between starter (read) and engine (apply).

## Modules

| File | Module | Contents |
|------|--------|---------|
| `loads_mod.F90` | `LOADS_MOD` | General load data type: load ID, type, node/group reference, function ID, scale |
| `domdec_load_mod.F` | `DOMDEC_LOAD_MOD` | Domain decomposition load data: per-rank load lists after METIS partitioning |
| `pblast_mod.F90` | `PBLAST_MOD` | Pressure blast (`/PBLAST`) data: ConWep parameters, burst point, scaled distance |
| `pload_cyl_mod.F` | `PLOAD_CYL_MOD` | Cylindrical pressure load parameters |
| `th_surf_mod.F` | — | (moved; see `interfaces/`) Time history surface module |

## Load Data Type

`LOADS_MOD` defines the generic `LOAD_DATA` structure shared by all load types:
- Load ID
- Load type (concentrated, pressure, gravity, centrifugal, blast)
- Target (node ID, group ID, or "all")
- Function ID (for time variation via `/FUNCT`)
- Scale factor and direction vector

The domain-decomposition module (`domdec_load_mod.F`) splits this load table across MPI ranks after METIS partitioning so each rank only evaluates loads applied to its local nodes.

## Related Documentation

- `common_source/modules/README.md` — parent modules directory
- `engine/source/loads/README.md` — load application in engine
- `starter/source/loads/README.md` — load reading in starter
