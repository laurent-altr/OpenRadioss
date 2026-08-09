# H3D Output Variable List (`engine/source/output/h3d/input_list/`)

Registers the H3D output variable list with the H3D library: one `h3d_list_*.F` routine per element type and result class declares what variables are available for output.

## Key Files

| File | Role |
|------|------|
| `h3d_gene_keyword.F` | Generate H3D keyword string for a result variable |
| `h3d_list_noda_scalar.F` | Register nodal scalar output variables |
| `h3d_list_noda_tensor.F` | Register nodal tensor output variables |
| `h3d_list_noda_vector.F` | Register nodal vector output variables |
| `h3d_list_shell_scalar.F` | Register shell element scalar output variables |
| `h3d_list_shell_tensor.F` | Register shell element tensor output variables |
| `h3d_list_shell_vector.F` | Register shell element vector output variables |
| `h3d_list_solid_scalar.F` | Register solid element scalar output variables |
| `h3d_list_solid_tensor.F` | Register solid element tensor output variables |
| `h3d_list_solid_vector.F` | Register solid element vector output variables |
| `h3d_list_quad_scalar.F` | Register 2D quad element scalar output variables |
| `h3d_list_quad_tensor.F` | Register 2D quad element tensor output variables |
| `h3d_list_quad_vector.F` | Register 2D quad element vector output variables |
| `h3d_list_1d_scalar.F` | Register 1D element scalar output variables |
| `h3d_list_1d_tensor.F` | Register 1D element tensor output variables |
| `h3d_list_1d_torsor.F` | Register 1D element torsor output variables |
| `h3d_list_1d_vector.F` | Register 1D element vector output variables |
| `h3d_list_sph_scalar.F` | Register SPH particle scalar output variables |
| `h3d_list_sph_tensor.F` | Register SPH particle tensor output variables |

## Description

These routines run once at H3D file initialisation. Each calls into the H3D library to declare the available result variables (with their names, units, and tensor rank) so that HyperView can correctly label and display them. The actual data is written by the `h3d_results/` routines at each output step.

## Related Documentation

- `engine/source/output/h3d/README.md` — parent H3D directory
- `engine/source/output/h3d/h3d_results/README.md` — per-step result writers
