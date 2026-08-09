# MPI Initialisation (`engine/source/mpi/init/`)

Initialises the MPI parallel environment at engine startup: sets up communication tables, domain neighbour lists, and ghost node mappings.

## Key Files

| File | Role |
|------|------|
| `inipar.F` | Main MPI initialisation: read domain boundaries, build neighbour lists |
| `init_global_boundary_list.F90` | Build list of inter-domain boundary nodes |
| `spmd_kill.F` | Graceful MPI abort on fatal error |
| `spmd_mstop.F` | Soft stop: synchronise all ranks and exit cleanly |
| `spmd_rst_check.F` | Check restart file consistency across MPI ranks |

## Algorithm

`inipar.F` reads the per-domain restart files written by the starter (`starter/source/restart/ddsplit/`) and builds:
1. The list of owned nodes and elements per domain
2. The communication pattern: which ranks share ghost nodes and in which direction
3. The pack/unpack index arrays used by the exchange routines each step

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `starter/source/restart/ddsplit/README.md` — domain split in starter
