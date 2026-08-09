# ALE Modules (`common_source/modules/ale/`)

Fortran 90 modules defining data types and global arrays for the ALE (Arbitrary Lagrangian-Eulerian) subsystem. These are shared between the starter (initialisation) and the engine (time integration).

## Modules

| File | Module | Contents |
|------|--------|---------|
| `ale_mod.F` | `ALE_MOD` | Main ALE data type: grid velocity arrays, mass flux tables, ALE node list |
| `ale_connectivity_mod.F` | `ALE_CONNECTIVITY_MOD` | ALE element connectivity: `IALE` (ALE element list), neighbour tables for flux computation |
| `ale_ebcs_mod.F` | `ALE_EBCS_MOD` | ALE boundary condition data: inlet/outlet conditions, prescribed grid velocity |
| `alefvm_mod.F` | `ALEFVM_MOD` | Finite-volume ALE data: cell-face connectivity for MUSCL flux computation |
| `alemuscl_mod.F` | `ALEMUSCL_MOD` | MUSCL advection workspace arrays: limited slopes, reconstructed states |
| `multi_fvm_mod.F90` | `MULTI_FVM_MOD` | Multi-fluid finite-volume module: volume fractions, interface states |

## Design

ALE modules separate the data layout from the algorithm implementation. Algorithms in `engine/source/ale/` and `starter/source/ale/` operate on data structures defined here. This makes the data structures consistent across the two binaries without code duplication.

## Related Documentation

- `common_source/modules/README.md` — parent modules directory
- `engine/source/ale/README.md` — ALE algorithm implementation
- `starter/source/ale/README.md` — ALE initialisation
