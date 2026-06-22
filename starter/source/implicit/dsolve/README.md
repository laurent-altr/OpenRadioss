# Implicit Solver Dimension (`starter/source/implicit/dsolve/`)

Starter sizing routines for the implicit linear solver.

## Key Files

| File | Role |
|------|------|
| `dsdim.F` | Compute dimension estimates for implicit solver arrays (stiffness matrix bandwidth, PCG workspace) |

## Description

`dsdim.F` analyses the mesh connectivity and DOF count to estimate the memory requirements for the implicit solver's sparse stiffness matrix (CSR bandwidth, nonzero count) and the preconditioned conjugate gradient workspace. The estimates are written to the restart file to guide engine memory allocation.

## Related Documentation

- `starter/source/implicit/README.md` — parent directory
- `engine/source/implicit/README.md` — implicit solver implementation
