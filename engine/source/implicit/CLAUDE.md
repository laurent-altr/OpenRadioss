# engine/source/implicit/

## Purpose
Implicit finite-element solver: builds and solves the global stiffness system
for quasi-static or implicit-dynamic analyses (`/IMPL/` keyword). Activated only
when `NDSOLV > 0`; the explicit engine (`RESOL`) delegates to these routines
when an implicit step is requested.

## Key files

| File | Role |
|------|------|
| `imp_init.F` | Implicit solver initialization: assembles initial stiffness matrix |
| `imp_solv.F` | Main implicit solve: Newton-Raphson iteration driver |
| `imp_dsolv.F` | Direct sparse solver interface (MUMPS or internal) |
| `imp_mumps.F` | MUMPS sparse direct solver interface |
| `imp_pcg.F` | Preconditioned Conjugate Gradient iterative solver |
| `imp_fsa_inv.F` | Full sparse assembly inverse |
| `imp_fac_ic.F` | Incomplete Cholesky factorization preconditioner |
| `imp_glob_k.F` | Assembles global stiffness matrix from element stiffness contributions |
| `ind_glob_k.F` | Index/sparsity pattern for global K |
| `imp_int_k.F` | Element-level stiffness contribution for implicit integration |
| `imp_dyna.F` | Implicit dynamics (Newmark scheme) |
| `imp_buck.F` | Buckling analysis (eigenvalue problem from K) |
| `imp_bfgs.F` | BFGS quasi-Newton update for non-linear implicit solve |
| `imp_dt.F` | Implicit time-step control |
| `imp_lanz.F` | Lanczos eigensolver for modal analysis |
| `imp_setb.F` | Sets boundary conditions in the implicit system |
| `imp_sol_init.F` | Solution vector initialization |
| `imp_pc_inv.F` | Inverse preconditioner |
| `imp_dsfext.F` | External force assembly for implicit |
| `lin_solv.F` | Linear solver dispatcher |
| `nl_solv.F` | Non-linear solver dispatcher |
| `prec_solv.F` | Preconditioner-based solver |
| `integrator.F` | Time integrator abstraction (Newmark / generalized-α) |
| `assem_s4.F` … `assem_c4.F` | Element stiffness assembly for specific element types (shells 4-node, 3-node, beam, spring, quad, solid10/20) |
| `cgshell.F` | Consistent geometric stiffness for shells (buckling) |
| `produt_v.F` | Matrix-vector product K·v |
| `recudis.F` | Displacement recovery after solve |
| `upd_glob_k.F` | Updates global K after state change |

## Integration
The implicit solver is called from `RESOL` when `NDSOLV > 0` (set by `/IMPL/`).
The explicit cycle normally runs until the implicit trigger, then switches to
Newton-Raphson iterations for equilibrium. The solution updates `NODES%X`, `NODES%V`,
`NODES%A` just like the explicit leapfrog.

## Dependencies
- Called by: `RESOL` (implicit section, gated by `NDSOLV`)
- MPI: `engine/source/mpi/implicit/`
- External: optionally links against MUMPS sparse solver library
