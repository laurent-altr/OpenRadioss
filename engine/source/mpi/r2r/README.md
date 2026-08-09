# MPI Rad2Rad Coupling Exchange (`engine/source/mpi/r2r/`)

SPMD communication for rad2rad (Radioss-to-Radioss co-simulation) inter-process coupling.

## Key Files

| File | Role |
|------|------|
| `spmd_exch_r2r_nl.F` | Exchange non-local (inter-instance) forces and displacements for rad2rad |
| `spmd_r2r.F` | Main rad2rad SPMD exchange: gather/scatter coupling boundary data |

## Description

Rad2rad co-simulation runs two (or more) Radioss engine instances concurrently, coupling them at shared boundaries. Within each instance the coupling boundary may span multiple MPI domains. `spmd_r2r.F` gathers the coupling boundary data (force/displacement) from all domains within one instance to rank 0, which then exchanges it with the other instance's rank 0 via the rad2rad network protocol.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/coupling/rad2rad/README.md` — rad2rad coupling
