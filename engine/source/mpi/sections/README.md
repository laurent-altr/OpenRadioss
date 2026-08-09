# MPI Section Force Exchanges (`engine/source/mpi/sections/`)

Cross-domain gather for cross-section resultant forces output (`/SECT`).

## Key Files

| File | Role |
|------|------|
| `spmd_section.F` | Gather cross-section force/moment contributions across MPI domains |

## Description

`/SECT` output computes resultant force and moment on a user-defined cross-section cut. When the cut plane passes through multiple MPI domains, each domain contributes the forces from its portion of the cut. `spmd_section.F` performs an MPI_Reduce to sum all domain contributions to rank 0 for output.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `starter/source/tools/sect/README.md` — section cut definition in starter
