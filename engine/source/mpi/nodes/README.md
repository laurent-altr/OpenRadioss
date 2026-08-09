# MPI Node Exchanges (`engine/source/mpi/nodes/`)

Core ghost-node state exchanges: accelerations, forces, velocities between MPI domains each time step.

## Key Files

| File | Role |
|------|------|
| `spmd_exch_a.F` | Exchange nodal accelerations (main ghost exchange) |
| `spmd_exch2_a_pon.F` | Secondary acceleration exchange for PON nodes |
| `spmd_exch_a_ams_poff.F` | AMS acceleration exchange with P-off correction |
| `spmd_exch_a_int2.F` | Acceleration exchange for TYPE2 interface nodes |
| `spmd_exch_a_int2_ams.F` | TYPE2 + AMS combined acceleration exchange |

## Algorithm

After the internal force assembly each step, ghost nodes need the forces accumulated by neighbouring domains. `spmd_exch_a.F` is the hot path: pack ghost-node accelerations into send buffers, MPI_Isend/Irecv, unpack and add received contributions. This is the dominant MPI communication in a typical explicit time step.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/mpi/forces/README.md` — force exchange
