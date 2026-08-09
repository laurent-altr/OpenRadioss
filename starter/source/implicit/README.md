# Starter Implicit Subsystem

This subsystem reads and validates implicit solver configuration keywords and initialises the implicit solver data structures.

## Directory Structure

```
implicit/
└── dsolve/    — Direct solver parameter setup
```

## Role

The starter's implicit subsystem processes `/IMPL/` keywords:
- `/IMPL/SOLVER` — solver type (PCG, MUMPS, built-in direct)
- `/IMPL/NEWM` — Newmark integration parameters (β, γ)
- `/IMPL/HHT` — HHT-α parameters
- `/IMPL/MODAL` — modal (Lanczos) analysis setup
- `/IMPL/BUCKL` — buckling analysis setup
- `/IMPL/MONIT` — convergence monitoring parameters

## Direct Solver Setup (`dsolve/`)

Initialises the direct linear solver (MUMPS or built-in):
- Reads solver parameters (fill-in factor, memory limit, pivot tolerance)
- Sets up the sparsity pattern structure for the global stiffness matrix
- Allocates the sparse matrix storage

The sparsity pattern is determined at startup from the mesh connectivity (which nodes are connected through which elements). This is expensive to compute, so it is done once in the starter and stored in the restart file.

## Related Documentation

- `engine/source/implicit/README.md` — runtime implicit time integration
