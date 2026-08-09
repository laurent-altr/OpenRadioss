# H3D Result Writers (`engine/source/output/h3d/h3d_results/`)

Per-element-type and per-variable result extraction and formatting routines that populate H3D output at each animation time point.

## Key Files

| File | Role |
|------|------|
| `genh3d.F` | Main H3D generation entry: loop over requested variables, dispatch per-type writers |
| `h3d_nodal_scalar.F` | Extract nodal scalar field (pressure, temperature, DT) |
| `h3d_nodal_tensor.F` | Extract nodal tensor field (stress at nodes) |
| `h3d_nodal_vector.F` | Extract nodal vector field (velocity, displacement) |
| `h3d_shell_scalar.F` / `h3d_shell_scalar_1.F` | Shell element scalar result (plastic strain, damage) |
| `h3d_shell_tensor.F` | Shell element tensor result (stress, strain) |
| `h3d_shell_vector.F` / `h3d_shell_vector_1.F` | Shell element vector result |
| `h3d_solid_scalar.F` / `h3d_solid_scalar_1.F` | Solid element scalar result |
| `h3d_solid_tensor.F` / `h3d_solid_tensor_1.F` | Solid element tensor result |
| `h3d_solid_vector.F` | Solid element vector result |
| `h3d_quad_scalar.F` / `h3d_quad_scalar_1.F90` | 2D quad element scalar result |
| `h3d_quad_tensor.F` | 2D quad element tensor result |
| `h3d_quad_vector.F` | 2D quad element vector result |
| `h3d_oned_scalar.F90` | 1D element scalar result (spring, beam, truss) |
| `h3d_oned_tensor.F` | 1D element tensor result |
| `h3d_oned_torsor.F` | 1D element force/moment torsor |
| `h3d_oned_vector.F` | 1D element vector result |
| `h3d_sph_scalar.F` | SPH particle scalar result |
| `h3d_sph_tensor.F` | SPH particle tensor result |
| `h3d_skin_scalar.F` / `h3d_skin_tensor.F` / `h3d_skin_vector.F` | Skin (surface output) results |
| `h3d_skin_ixskin.F` | Skin element index mapping |
| `h3d_skin_off.F` | Skin output offset computation |
| `h3d_skin_pre_map.F` | Pre-map skin element connectivity |
| `h3d_sol_skin_scalar.F` / `h3d_sol_skin_scalar1.F` / `h3d_sol_skin_tensor.F` | Solid skin (surface of solid) results |
| `h3d_sol_skin_ixskin.F` | Solid skin element index mapping |
| `h3d_fld_strain.F` | FLD (Forming Limit Diagram) strain output |
| `h3d_fld_tsh.F` | FLD strain for thick-shell elements |
| `h3d_strn_tenscor3.F` | Strain tensor corner node extraction |
| `h3d_velvecc22.F` / `h3d_velvecz22.F` | Velocity vector for ALE/TYPE22 elements |
| `h3d_write_scalar.F` | Low-level H3D scalar write |
| `h3d_write_tensor.F` / `h3d_write_tensor_corner.F` | Low-level H3D tensor write |
| `h3d_write_sh_tensor.F` / `h3d_write_sh_tensor_array.F` | H3D shell tensor write |
| `h3d_write_torsor.F` | Low-level H3D torsor write |
| `h3d_write_vector.F` / `h3d_write_vectors.F` | Low-level H3D vector write |
| `fcont_max_output.F` | Contact force maximum output |
| `fcont2_max_output.F` / `fcont2_min_output.F` | Contact2 force max/min output |
| `fpcont2_max_output.F90` / `fpcont2_min_output.F90` | Fluid-pressure contact max/min output |
| `strs_tenscor3.F` | Stress tensor corner-node extrapolation |

## Related Documentation

- `engine/source/output/h3d/README.md` — parent H3D directory
- `engine/source/output/h3d/h3d_build_fortran/README.md` — result packaging
- `engine/source/output/h3d/input_list/README.md` — output variable list management
