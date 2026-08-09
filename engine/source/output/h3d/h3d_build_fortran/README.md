# H3D Fortran Result Builders (`engine/source/output/h3d/h3d_build_fortran/`)

Fortran routines that package simulation result arrays into the H3D binary output format via the C++ API layer.

## Key Files

| File | Role |
|------|------|
| `create_h3d_1d_scalar.F` | Write 1D element scalar result (e.g., spring force) to H3D |
| `create_h3d_1d_tensor.F` | Write 1D element tensor result to H3D |
| `create_h3d_1d_torsor.F` | Write force/moment torsor (section resultants) to H3D |
| `create_h3d_1d_vector.F` | Write 1D element vector result to H3D |
| `create_h3d_arg_keyword.F` | Write H3D argument keyword (result variable name) |
| `create_h3d_input.F` | Write input deck reference to H3D file |
| `create_h3d_nodal_scalar.F` | Write nodal scalar result (e.g., pressure, temperature) to H3D |
| `create_h3d_nodal_tensor.F` | Write nodal tensor result to H3D |
| `create_h3d_nodal_vector.F` | Write nodal vector result (e.g., velocity, displacement) to H3D |
| `create_h3d_output_per_part.F90` | Write per-part result arrays to H3D |

## Description

These routines are called from `genh3d.F` (in `h3d_results/`) at each output time point. They convert Radioss internal arrays (element buffer, nodal arrays) to H3D datatypes and invoke the C++ H3D library calls via wrappers in `h3d_build_cpp/`.

## Related Documentation

- `engine/source/output/h3d/README.md` — parent H3D directory
- `engine/source/output/h3d/h3d_build_cpp/README.md` — C++ API wrappers
- `engine/source/output/h3d/h3d_results/README.md` — result selection and dispatch
