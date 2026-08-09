# Node Partitioning (`starter/source/spmd/node/`)

Assigns nodes to MPI ranks after domain decomposition and builds ghost node lists.

## Key Files

| File | Role |
|------|------|
| `ddtools.F` | Domain decomposition tools: utility routines for rank assignment and list manipulation |
| `frontplus.F` | Build the communication front: identify boundary nodes shared between ranks (ghost nodes) |
| `st_array_size.F` | Compute array sizes needed for MPI communication buffers |

## Ghost Node Concept

After METIS assigns elements to ranks, nodes on rank boundaries are shared. Each shared node is:
- **Local** on one rank (the "owner" rank — computes force)
- **Ghost** on neighbouring ranks (receives owner's result via MPI)

`frontplus.F` identifies all ghost nodes: for each local element, any node assigned to a different rank is a ghost. The ghost node list drives the MPI `SPMD_ALLREDUCE`/`SPMD_SEND` communication pattern in the engine.

## Related Documentation

- `starter/source/spmd/README.md` — parent SPMD directory
- `starter/source/spmd/domain_decomposition/README.md` — element-to-rank assignment
- `engine/source/mpi/README.md` — MPI communication using ghost lists
