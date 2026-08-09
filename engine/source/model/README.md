# Model Subsystem

This subsystem manages the global finite element model data — the data structures that hold the complete problem definition: nodes, elements, connectivity, and adaptive mesh refinement.

## Directory Structure

```
model/
└── remesh/        — Adaptive remeshing utilities
```

## Role

The `model/` subsystem acts as the data layer of the engine. All other subsystems access the model through the module-level arrays defined here or in `engine/source/modules/`. The model data is read from the `_0001.rad` restart file at startup and updated in place throughout the simulation.

## Adaptive Remeshing (`remesh/`)

The `remesh/` subdirectory implements adaptive mesh refinement (AMR) for ALE and shell elements. It is activated by the `/ADMAS` (adaptive mass) or remeshing keywords.

| File | Role |
|------|------|
| `admini.F` | AMR initialisation — build refinement data structures |
| `admdiv.F` | Element division — split elements for refinement |
| `admfor0.F` | Force correction after refinement |
| `admgvid.F` | Geometry validity check after refinement |
| `admerr.F` | Error estimation (drives refinement decision) |

### Refinement Criterion

Elements are selected for refinement based on a user-defined error estimator (e.g. plastic strain gradient, pressure gradient). Elements above the threshold are split; their children inherit the solution by interpolation.

## Global Model Arrays

The engine model data is stored in large global arrays passed through Fortran argument lists or accessed via `USE` statements:

| Array (conventional name) | Contents |
|--------------------------|---------|
| `X(3, NUMNOD)` | Node coordinates (updated each step) |
| `V(3, NUMNOD)` | Node velocities |
| `A(3, NUMNOD)` | Node accelerations |
| `F(3, NUMNOD)` | Node force vector |
| `AM(NUMNOD)` | Nodal masses |
| `IRECT(MAXNOD, NUMEL)` | Element connectivity arrays |
| `PM(MAXPM, NUMMAT)` | Real property/material parameters |
| `IPM(MAXIPM, NUMMAT)` | Integer property/material parameters |

These conventions are inherited from the legacy Radioss code. Modern additions use Fortran derived types and modules.

## Related Documentation

- `engine/source/README.md` — overall engine architecture
- `engine/source/modules/README.md` — Fortran module definitions for model data
- `engine/source/ale/README.md` — ALE rezoning (a related mesh update operation)
