# Material Tool Utilities (`engine/source/materials/tools/`)

Mathematical and interpolation utilities shared across material law implementations.

## Key Files

| File | Role |
|------|------|
| `kmatinv.F` | Small matrix inverse (3×3) for material tangent |
| `prodAAT.F` / `prodATA.F` / `prodmat.F` | Matrix-matrix products: A×Aᵀ, Aᵀ×A, A×B |
| `read_mat_table.F` | Read and store material tabulated data (flow curves, etc.) |
| `roto_tens2d.F` / `roto_tens2d_aniso.F` | 2D tensor rotation (isotropic / anisotropic) |
| `table_mat_vinterp.F` | Vectorised 2D table interpolation for material flow curves |

## Description

These are low-level utilities called from within material law routines:
- Matrix operations (`kmatinv`, `prodmat`) are used for elasticity tensor inversion and tangent moduli in implicit laws
- `table_mat_vinterp.F` provides the inner-loop table lookup (strain rate × temperature flow stress tables) that is performance-critical for metals laws

## Related Documentation

- `engine/source/materials/mat_share/README.md` — material dispatch
