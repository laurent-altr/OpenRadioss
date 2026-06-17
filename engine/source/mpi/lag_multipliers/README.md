# MPI Lagrange Multiplier Exchanges (`engine/source/mpi/lag_multipliers/`)

Cross-domain communication for Lagrange multiplier constraint forces.

## Key Files

| File | Role |
|------|------|
| `spmd_lag.F` | Exchange Lagrange multiplier constraint forces across domain boundaries |
| `spmd_spamaj.F` | Sparse matrix-vector multiply for distributed Lagrange multiplier system |

## Description

Lagrange multiplier constraints (joints, MPC gear/rack constraints) involve constraint equations that may couple nodes in different MPI domains. `spmd_lag.F` exchanges the constraint forces across domain boundaries after the Lagrange multiplier solve. `spmd_spamaj.F` implements a distributed sparse matrix-vector product for the constraint system Jacobian used in the multiplier solve.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `starter/source/tools/lagmul/README.md` — Lagrange multiplier setup
