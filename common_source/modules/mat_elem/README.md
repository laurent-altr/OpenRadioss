# Material-Element Parameter Modules (`common_source/modules/mat_elem/`)

Fortran 90 modules defining the data types for material and element parameters stored in the `ELBUF`.

## Modules

| File | Module | Contents |
|------|--------|---------|
| `elbufdef_mod.F90` | `ELBUFDEF_MOD` | Element buffer layout constants: offsets within ELBUF for stress, strain, history variables |
| `eos_param_mod.F90` | `EOS_PARAM_MOD` | EOS parameter data type: arrays `PM_EOS` and `IPM_EOS` |
| `fail_param_mod.F90` | `FAIL_PARAM_MOD` | Failure criterion parameter data type |
| `matparam_def_mod.F90` | `MATPARAM_DEF_MOD` | Combined material parameter descriptor: elastic, plastic, EOS, failure |
| `mat_elem_mod.F90` | `MAT_ELEM_MOD` | Material-element pairing: maps element groups to material parameter arrays |
| `prop_param_mod.F90` | `PROP_PARAM_MOD` | Property parameter data type (shell thickness, integration rule, etc.) |
| `ply_param_mod.F90` | `PLY_PARAM_MOD` | Composite ply parameter data type (angle, thickness, material per ply) |
| `group_param_mod.F90` | `GROUP_PARAM_MOD` | Element group parameters (group ID, element count, buffer offsets) |
| `visc_param_mod.F90` | `VISC_PARAM_MOD` | Viscosity model parameters (Prony series, Kelvin-Voigt) |
| `therm_param_mod.F90` | `THERM_PARAM_MOD` | Thermal material parameters (conductivity, specific heat, expansion) |
| `glob_therm_mod.F90` | `GLOB_THERM_MOD` | Global thermal arrays: nodal temperature, heat flux |
| `sph_work.F90` | `SPH_WORK_MOD` | SPH working arrays: smoothing length, neighbour lists |

## Role

These modules replace the legacy `PM(NPROPELL, IMAT)` / `IPM(NIPROPELL, IMAT)` flat arrays with typed, named data structures. New material and element code should use these modules. The typed modules make the parameter layout self-documenting and reduce out-of-bounds bugs from integer index arithmetic.

## Related Documentation

- `common_source/modules/README.md` — parent modules directory
- `engine/source/materials/README.md` — material laws access PM/IPM via these types
