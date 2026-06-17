# MPI Fluid (CFD) Exchanges (`engine/source/mpi/fluid/`)

Cross-domain communication for the Navier-Stokes fluid solver.

## Key Files

| File | Role |
|------|------|
| `spmd_cfd.F` | Main CFD MPI exchange: velocity, pressure, temperature at domain interfaces |
| `spmd_check_ale_neighbour.F` | Verify ALE neighbour cell consistency across domains |
| `spmd_exchange_grad.F` | Exchange gradient fields (velocity/temperature gradients) at domain boundaries |

## Description

The CFD fluid solver requires consistent gradient computation across domain boundaries. `spmd_exchange_grad.F` exchanges gradients of flow quantities (velocity divergence, temperature) at ghost cells. `spmd_cfd.F` is the main exchange called at each CFD sub-step for pressure and velocity fields.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/fluid/README.md` — Navier-Stokes fluid solver
