# Linear Algebra Utilities (common_source/linearalgebra)

This directory provides basic Fortran module types for matrix and vector operations, shared between the starter and engine.

## Files

| File | Module | Purpose |
|------|--------|---------|
| `matrix_mod.F` | `MATRIX_MOD` | Dense matrix type with basic operations |
| `vector_mod.F` | `VECTOR_MOD` | Dense vector type with basic operations |

## Design Philosophy

These modules provide lightweight, typed wrappers around allocatable Fortran arrays. They are intended for **small, dense systems** (e.g. 3×3 rotation matrices, 6-component stress/strain vectors) — not for large sparse systems.

For large sparse linear systems (implicit solver, AMS), see:
- `engine/source/implicit/` — sparse PCG and MUMPS solvers
- `engine/source/ams/` — SMS PCG solver

## Usage

```fortran
use MATRIX_MOD, only : matrix_type
use VECTOR_MOD, only : vector_type
```

Both modules use `real(kind=WP)` (from `PRECISION_MOD`) for all floating-point data, ensuring compatibility with both single- and double-precision builds.

## Related Documentation

- `common_source/modules/` — precision and constant definitions
- `engine/source/implicit/README.md` — large sparse system solvers
