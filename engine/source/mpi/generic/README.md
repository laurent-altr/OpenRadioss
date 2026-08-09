# MPI Generic Utilities (`engine/source/mpi/generic/`)

General-purpose MPI utility routines shared across all parallel communication modules.

## Key Files

| File | Role |
|------|------|
| `glob_min.F` | Global minimum across all MPI ranks |
| `glob_minv.F` | Global minimum with associated value |
| `rad_spmd_recv.F` | Radioss wrapper for MPI_Recv |
| `rad_spmd_send.F` | Radioss wrapper for MPI_Send |
| `spmd_allgather.F90` | Wrapper for MPI_Allgather |

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
