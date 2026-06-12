# linearalgebra/

## Purpose
Sparse matrix and dense vector types for linear system solvers, primarily used by constraint formulations such as RBE3 rigid body elements.

## Files

| File | Description |
|------|-------------|
| `matrix_mod.F` | Module `MATRIX_MOD` — type `t_cfs_matrix` (Coordinate Format Sparse matrix with row/column index arrays and values); type-bound procedures: `matrix_create`, `matrix_destroy`, `matrix_associate`, `get_dim`, `prod_vec` (matrix-vector product) |
| `vector_mod.F` | Module `VECTOR_MOD` — type `t_vector` (sparse/dense vector with row indices and values); type-bound procedures: `create`, `destroy`, `associate`, `get_dim`, `set_dim`, `norm` |

## Key Types Exported
- **`t_cfs_matrix`** (`matrix_mod.F`) — coordinate-format sparse matrix with OOP interface
- **`t_vector`** (`vector_mod.F`) — sparse/dense vector with OOP interface

## Dependencies
- Uses: `modules/precision_mod.F90` (precision `WP`)
- Used by: constraint solvers (RBE3, etc.) in the engine
