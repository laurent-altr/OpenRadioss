# MPI / SPMD Parallelisation Subsystem

This subsystem implements the distributed-memory parallelisation of the engine using MPI (Message Passing Interface). The domain is decomposed at startup by the starter; the engine operates on sub-domains and exchanges ghost-layer data each time step.

## Architecture

OpenRadioss uses a **SPMD** (Single Program Multiple Data) parallel model:
- Each MPI rank owns a sub-domain of elements and their associated nodes.
- **Ghost nodes** (nodes shared between neighbouring sub-domains) hold copies of nodal data from adjacent ranks.
- Forces and velocities on ghost nodes are exchanged via point-to-point MPI messages each cycle.

## SPMD API (`spmd_mod.F90`)

All MPI calls in the engine go through the wrapper module `SPMD_MOD` (defined in `spmd_mod.F90`). The `SPMD_*` functions mirror `MPI_*` but provide:
- Optional arguments for communicator and status
- Fortran-friendly interfaces
- Consistent error handling

| SPMD function | Underlying MPI call |
|--------------|---------------------|
| `SPMD_ALLREDUCE` | `MPI_Allreduce` |
| `SPMD_ALLGATHERV` | `MPI_Allgatherv` |
| `SPMD_SEND` / `SPMD_RECV` | `MPI_Send` / `MPI_Recv` |
| `SPMD_ISEND` / `SPMD_IRECV` | `MPI_Isend` / `MPI_Irecv` |
| `SPMD_WAIT` | `MPI_Wait` |
| `SPMD_PACK` / `SPMD_UNPACK` | `MPI_Pack` / `MPI_Unpack` |
| `SPMD_IALLREDUCE` | `MPI_Iallreduce` (non-blocking) |

**Always use `SPMD_*` functions, never call `MPI_*` directly in engine code.**

## Directory Structure

```
mpi/
├── airbags/           — MPI exchanges for airbag control volumes
├── ale/               — ALE/Euler inter-domain flux communication
├── ams/               — AMS mass scaling MPI operations
├── anim/              — Animation output coordination across ranks
├── elements/          — Element-level ghost force exchanges
├── fluid/             — Fluid domain inter-rank communication
├── forces/            — Nodal force reduction across ghost nodes
├── generic/           — Generic send/receive and global min/max utilities
├── implicit/          — Implicit solver distributed assembly and solve
├── init/              — MPI initialisation and topology setup
├── interfaces/        — Contact interface SPMD communication
├── kinematic_conditions/ — Kinematic constraint MPI enforcement
├── lag_multipliers/   — Lagrange multiplier SPMD
├── nodes/             — Nodal data exchange (positions, velocities)
├── output/            — Parallel output coordination
├── r2r/               — Rank-to-rank direct communication utilities
├── seatbelts/         — Seatbelt element MPI
├── sections/          — Section force MPI reduction
├── sph/               — SPH inter-rank particle exchange
└── user_interface/    — User subroutine MPI wrappers
```

Top-level files:
- `spmd_mod.F90` — Main SPMD wrapper module
- `spmd_allgatherv.F90`, `spmd_allreduce.F90` — Collective operations
- `spmd_comm_world.F90` — Communicator management
- `spmd_constants.F90` — MPI constants and tags
- `spmd_error.F90` — MPI error handling
- `spmd_exch_sub.F` — Sub-domain exchange dispatcher
- `spmd_isend.F90`, `spmd_irecv.F90` — Non-blocking point-to-point
- `spmd_send.F90`, `spmd_recv.F90` — Blocking point-to-point
- `spmd_pack.F90`, `spmd_unpack.F90` — Message packing
- `spmd_wait.F90` — Wait for non-blocking operations
- `python_spmd_mod.F90` — Python/ctypes bridge for SPMD (experimental)
- `get_mpi_operator.F90` — Map reduction operations to MPI operators

## Communication Pattern Per Time Step

```
1. Element force computation (local, no MPI)
2. Force scatter to nodes (local)
3. Ghost force reduction:
       mpi/forces/ → SPMD_ALLREDUCE on ghost node forces
4. Velocity update (local)
5. Ghost velocity exchange:
       mpi/nodes/ → SPMD_SEND/RECV neighbour velocities
6. Contact interface exchange:
       mpi/interfaces/ → exchange penetration / force across domain boundaries
7. Output reduction:
       mpi/output/ → global min/max, sum for energy balance
```

## Domain Decomposition

The starter partitions the mesh into sub-domains (using graph partitioning). Each rank receives:
- A list of owned elements and nodes
- Ghost node indices and their owning ranks
- Send/receive maps for each communication step

This topology is fixed for the duration of the run (no dynamic load balancing).

## OpenMP + MPI Hybrid

Within each MPI rank, element loops are parallelised with OpenMP threads. The MPI layer operates between OpenMP parallel regions (in the master thread). This is a flat MPI+OpenMP model.

## Python Interface (`python_spmd_mod.F90`)

An experimental ctypes-compatible interface exposing SPMD operations to Python scripts. Used for co-simulation and workflow automation.

## Related Documentation

- `engine/source/assembly/README.md` — force scatter before ghost reduction
- `engine/source/README.md` — time loop structure
- `.github/copilot-instructions.md` — MPI section: always use `SPMD_MOD`
