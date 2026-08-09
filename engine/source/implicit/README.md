# Implicit Solver Subsystem

This subsystem implements implicit time integration for quasi-static and dynamic analyses. It provides both linear and nonlinear solution procedures, multiple linear solvers, and eigenvalue analysis.

## Overview

OpenRadioss's primary integration scheme is explicit, but the implicit solver activates when `/IMPL/SOLVER` or quasi-static loading is requested. The implicit solver iterates until the residual force is below tolerance each time step, using a much larger `dt` than the explicit scheme allows.

## Key Files

| File | Role |
|------|------|
| `integrator.F` | Top-level implicit time integrator (Newmark, HHT-α) |
| `nl_solv.F` | Nonlinear Newton-Raphson iteration loop |
| `lin_solv.F` | Linear system dispatch — selects solver backend |
| `imp_init.F` | Implicit solver initialisation |
| `imp_sol_init.F` | Linear system initialisation |
| `imp_glob_k.F` | Global stiffness matrix assembly |
| `imp_int_k.F` | Element stiffness matrix integration |
| `ind_glob_k.F` | Stiffness matrix index / sparsity management |
| `upd_glob_k.F` | Stiffness update after Newton iteration |
| `imp_dsfext.F` | External force vector assembly |
| `imp_dyna.F` | Dynamic implicit (Newmark) update |
| `imp_dt.F` | Implicit time step control |
| `prec_solv.F` | Preconditioner for iterative solver |
| `imp_pcg.F` | Preconditioned Conjugate Gradient (PCG) linear solver |
| `imp_mumps.F` | Interface to MUMPS direct sparse solver |
| `imp_fac_ic.F` | Incomplete Cholesky factorisation (preconditioner) |
| `imp_fsa_inv.F` | Forward/backward substitution |
| `imp_bfgs.F` | BFGS quasi-Newton update |
| `imp_lanz.F` | Lanczos eigensolver (for modal analysis / buckling) |
| `imp_buck.F` | Buckling analysis |
| `imp_setb.F` | Boundary condition enforcement in implicit system |
| `imp_pc_inv.F` | Preconditioned inverse iteration |
| `recudis.F` | Recovery of displacements after solve |
| `produt_v.F` | Sparse matrix-vector product |
| `cgshell.F` | Shell element stiffness integration helper |
| `assem_*.F` | Element-type-specific stiffness assembly (s4, q4, c3, c4, …) |

## Integration Schemes

| Scheme | Control parameter | Notes |
|--------|-----------------|-------|
| Newmark-β | `IBETA`, `IGAMMA` | Unconditionally stable for β ≥ 0.25 |
| HHT-α | `IALFA` | Numerical damping of high-frequency modes |
| Quasi-static | `INSTAT=1` | No inertia; static equilibrium each step |

## Linear Solver Backends

| Solver | File | Notes |
|--------|------|-------|
| PCG | `imp_pcg.F` | Iterative; good for large sparse systems |
| MUMPS | `imp_mumps.F` | Direct; requires MUMPS library at link time |
| Built-in direct | `imp_fsa_inv.F` | Internal factorisation (small models) |

The solver is selected via `/IMPL/SOLVER/MUMPS` or `/IMPL/SOLVER/ITER`.

## Nonlinear Iteration

`nl_solv.F` drives the Newton-Raphson loop:

```
for each implicit step:
  while (||residual|| > tolerance):
    assemble K_global (imp_glob_k.F)
    assemble F_ext - F_int  (residual)
    solve  K · Δu = residual  (lin_solv.F)
    update  u += Δu
    update element stresses (element loop)
```

BFGS quasi-Newton updates (`imp_bfgs.F`) can replace full tangent stiffness rebuilds to reduce cost.

## Eigenvalue / Modal Analysis

The Lanczos algorithm (`imp_lanz.F`) extracts natural frequencies and mode shapes. Used by `/IMPL/MODAL` and for buckling analysis (`imp_buck.F`).

## MPI Considerations

The global stiffness matrix is assembled in distributed form. Ghost DOF communication is handled through the MPI layer (`mpi/implicit/`). The PCG solver operates on the distributed system; MUMPS handles its own internal distribution.

## Related Documentation

- `engine/source/assembly/README.md` — explicit force assembly (shared data structures)
- `engine/source/elements/README.md` — element stiffness contributions (`assem_*.F`)
- `engine/source/mpi/README.md` — distributed memory parallelism
