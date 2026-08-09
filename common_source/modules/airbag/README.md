# Airbag Modules (`common_source/modules/airbag/`)

Fortran 90 modules defining data types for the airbag subsystem. Shared between starter and engine.

## Modules

| File | Module | Contents |
|------|--------|---------|
| `fvmbag_meshcontrol_mod.F` | `FVMBAG_MESHCONTROL_MOD` | Finite-volume airbag mesh control parameters: cell size limits, rezoning triggers |

## Context

The finite-volume (FV) airbag model (`/MONVOL/FVMBAG`) uses an internal mesh to model the gas flow inside the airbag during inflation. `FVMBAG_MESHCONTROL_MOD` stores parameters governing this internal mesh:
- Minimum and maximum cell sizes
- Rezoning frequency
- Interface resolution between gas zones

These parameters are read by the starter during airbag initialisation and used by the engine during the FV pressure computation each time step.

## Related Documentation

- `common_source/modules/README.md` — parent modules directory
- `engine/source/airbag/README.md` — FV airbag implementation (`fvbag*.F`)
- `starter/source/airbag/README.md` — airbag initialisation
