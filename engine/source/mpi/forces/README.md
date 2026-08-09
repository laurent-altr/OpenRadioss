# MPI Force and Reduction (`engine/source/mpi/forces/`)

Global force reductions and nodal force exchanges across MPI domains.

## Key Files

| File | Role |
|------|------|
| `spmd_exch_a.F` (via nodes/) | Nodal acceleration exchange (see nodes/) |
| `glob_min.F` | Global minimum reduction (MPI_Allreduce MIN) |
| `glob_minv.F` | Global minimum with value (index + value) reduction |
| `rad_spmd_recv.F` / `rad_spmd_send.F` | Radioss-specific MPI send/recv wrappers |
| `spmd_allgather.F90` | Wrapper for MPI_Allgather operations |

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/mpi/nodes/README.md` — nodal acceleration exchange
