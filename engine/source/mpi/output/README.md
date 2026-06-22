# MPI Output (`engine/source/mpi/output/`)

Gathers distributed result data from all MPI domains to rank 0 for writing output files.

## Key Files

| File | Role |
|------|------|
| `adler32.cpp` | Adler-32 checksum for output determinism checks |
| `node_checksum.F` | Nodal state checksum computation |
| `spmd_collect.F` | Gather scalar/vector result arrays from all ranks |
| `spmd_collect_multi_fvm.F` | Gather FVM multi-material cell results |
| `spmd_collect_nlocal.F` | Gather non-local averaging quantities |

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/output/h3d/spmd/README.md` — H3D-specific gather routines
