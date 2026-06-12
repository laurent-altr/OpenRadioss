# modules/mat_elem/

## Purpose
Material and element parameter type definitions used by all material law implementations in the engine and starter. Provides master parameter structures (material, EOS, failure, viscosity, thermal, property) and SPH work arrays. This is one of the most widely depended-on directories in `common_source`.

## Files

### Master parameter definitions
| File | Module | Description |
|------|--------|-------------|
| `matparam_def_mod.F90` | `MATPARAM_DEF_MOD` | Master material parameter type definitions; most widely referenced material module throughout the codebase |
| `elbufdef_mod.F90` | `ELBUFDEF_MOD` | Element buffer definitions; extensively referenced by element processing routines |
| `mat_elem_mod.F90` | `MAT_ELEM_MOD` | Links `elbufdef`, `group_param`, `matparam_def`, and `prop_param` modules into combined material-element data |
| `eos_param_mod.F90` | `EOS_PARAM_MOD` | EOS parameter structures; referenced by all EOS implementations in `eos/` |
| `fail_param_mod.F90` | `FAIL_PARAM_MOD` | Failure model parameter structures |

### Per-model parameter types
| File | Module | Description |
|------|--------|-------------|
| `visc_param_mod.F90` | `VISC_PARAM_MOD` | Type `visc_param_`: viscosity law type, title, parameters, function tables, internal tables |
| `ply_param_mod.F90` | `PLY_PARAM_MOD` | Type `ply_param_`: composite ply data (angle, thickness, position, material IDs) |
| `prop_param_mod.F90` | `PROP_PARAM_MOD` | Type `prop_param_`: element property (title, property ID, layers, geometry arrays) |
| `therm_param_mod.F90` | `THERM_PARAM_MOD` | Type `therm_param_`: thermal model (temperature, conductivity, expansion, phase transition properties) |
| `group_param_mod.F90` | `GROUP_PARAM_MOD` | Type `group_param_`: element group parameters (material, property, strain flag, viscosity coefficients) |
| `glob_therm_mod.F90` | `GLOB_THERM_MOD` | Global thermal module data |

### Utility
| File | Module | Description |
|------|--------|-------------|
| `sph_work.F90` | `SPH_WORK` | Types `sph_work_voxel_` and `sph_work_`: SPH neighbor-search voxels, gradients, and interpolation working buffers |

## Key Types Exported
- **`matparam_def`** types — material law parameters (widely used)
- **`eos_param_`** — EOS parameters passed to `eos/eosmain.F`
- **`group_param_`** — per-element-group material/property data
- **`visc_param_`**, **`ply_param_`**, **`prop_param_`**, **`therm_param_`** — specialized parameter containers

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: all material law implementations, element processing, EOS dispatcher, and failure criteria routines
