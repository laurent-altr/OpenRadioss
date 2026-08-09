# MPI Implicit Solver Exchanges (`engine/source/mpi/implicit/`)

Cross-domain communication for the implicit solver: stiffness matrix assembly and linear solver interface.

## Key Files

| File | Role |
|------|------|
| `imp_fri.F` | Implicit friction force exchange across domains |
| `imp_spmd.F` | Main implicit SPMD exchange: residual and stiffness matrix |
| `spmd_dsreso.F` | Distributed sparse residual vector exchange |

## Description

The implicit solver requires assembling a global stiffness matrix from distributed element contributions. `imp_spmd.F` handles the MPI communication for the distributed sparse stiffness assembly: ghost-node stiffness contributions are exchanged between domain boundaries and added to the local stiffness matrix. `spmd_dsreso.F` handles residual vector reduction across domains for the Newton convergence check.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/implicit/README.md` — implicit solver
