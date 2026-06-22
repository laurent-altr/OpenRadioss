# Common Modules (common_source/modules)

This directory is the most widely used in the entire codebase. It defines the Fortran modules that are `USE`d by virtually every file in the starter and engine. If you are adding new code, you will almost certainly need to use modules from here.

## Core Modules (top-level files)

| File | Module | Purpose |
|------|--------|---------|
| `precision_mod.F90` | `PRECISION_MOD` | Working precision kind `WP` (4 or 8) — use `real(kind=WP)` everywhere |
| `constant_mod.F` | `CONSTANT_MOD` | Mathematical constants (`PI`) and physical constants |
| `array_mod.F` | `ARRAY_MOD` | Dynamic array helpers (allocation, resizing) |
| `cast_mod.F90` | `CAST_MOD` | Type-safe casting utilities |
| `check_mod.F` | `CHECK_MOD` | Assertion and bounds-check utilities (debug mode) |
| `connectivity.F90` | `CONNECTIVITY_MOD` | Node-element connectivity data structures |
| `nodal_arrays.F90` | `NODAL_ARRAYS_MOD` | Global nodal arrays (coordinates, velocities, forces) |
| `groupdef_mod.F` | `GROUPDEF_MOD` | Group (set) definitions and look-up |
| `setdef_mod.F` | `SETDEF_MOD` | Set definitions (node sets, element sets) |
| `sensor_mod.F90` | `SENSOR_MOD` | Sensor data structures and evaluation |
| `skew_mod.F90` | `SKEW_MOD` | Skew frame / local coordinate system definitions |
| `inoutfile_mod.F` | `INOUTFILE_MOD` | Input/output file unit management |
| `names_and_titles_mod.F` | `NAMES_AND_TITLES_MOD` | String length constants (`ncharline100`, …) |
| `optiondef_mod.F` | `OPTIONDEF_MOD` | Option flags and control booleans |
| `state_mod.F` | `STATE_MOD` | Global simulation state flags |
| `unitab_mod.F` | `UNITAB_MOD` | Unit conversion table |
| `outmax_mod.F` | `OUTMAX_MOD` | Output maximum value tracking |
| `inoutfile_mod.F` | `INOUTFILE_MOD` | File unit numbers |
| `inivel_mod.F90` | `INIVEL_MOD` | Initial velocity definitions |
| `seatbelt_mod.F` | `SEATBELT_MOD` | Seatbelt / retractor state |
| `spring_functions_mod.F90` | `SPRING_FUNCTIONS_MOD` | Spring force-displacement functions |
| `failwave_mod.F` | `FAILWAVE_MOD` | Failure wave tracking |
| `xfem2def_mod.F` | `XFEM2DEF_MOD` | XFEM crack definitions |
| `nlocal_reg_mod.F` | `NLOCAL_REG_MOD` | Non-local regularisation data |
| `pinchtype_mod.F` | `PINCHTYPE_MOD` | Pinch (edge) node type flags |
| `parith_on_mod.F90` | `PARITH_ON_MOD` | Parallel arithmetic control flags |
| `brokmann_random_def_mod.F90` | `BROKMANN_RANDOM_DEF_MOD` | Brokmann random field |
| `random_walk_def_mod.F90` | `RANDOM_WALK_DEF_MOD` | Random walk model |
| `mds_rest.mod.F` | `MDS_REST_MOD` | Modal dynamics restart |
| `multimat_param_mod.F90` | `MULTIMAT_PARAM_MOD` | Multi-material parameter table |
| `table4d_mod.F` | `TABLE4D_MOD` | 4D tabulated data look-up |
| `root_finding_algo_mod.F90` | `ROOT_FINDING_ALGO_MOD` | Root-finding algorithms (bisection, Newton) |
| `plot_curve_mod.F` | `PLOT_CURVE_MOD` | Curve plotting for diagnostic output |
| `element_user_id.F90` | `ELEMENT_USER_ID_MOD` | User ID ↔ internal ID mapping for elements |
| `user_windows_mod.F` | `USER_WINDOWS_MOD` | User window (region) definitions |

## Subdirectory Modules

| Subdirectory | Contents |
|-------------|---------|
| `ale/` | `ale_mod.F`, `alemuscl_mod.F`, `alefvm_mod.F`, `ale_connectivity_mod.F`, `ale_ebcs_mod.F` — ALE data structures |
| `airbag/` | Airbag / control volume module definitions |
| `boundary_conditions/` | BC state and type modules |
| `constraints/` | Rigid body and constraint modules |
| `elements/` | `element_mod.F90`, `sfem_mod.F90` — element and SFEM modules |
| `interfaces/` | Contact interface module definitions |
| `loads/` | Load type and state modules |
| `mat_elem/` | Material-element coupling modules |
| `output/` | Output request modules |

## Python Integration (`python_mod.F90`, `cpp_python_funct.cpp`)

These files provide the bridge between OpenRadioss and Python co-simulation:
- `python_mod.F90` — Fortran module to call Python-defined load curves and callbacks
- `python_element_mod.F90` — Python-defined element formulations
- `cpp_python_funct.cpp` — C++ implementation using the CPython API
- `python_signal.h` / `cpp_python_sampling.h` — C++ headers for Python integration

## Most Important Modules for New Contributors

If you are writing new engine or starter code, you will most often need:

```fortran
use PRECISION_MOD, only : WP            ! always: real(kind=WP)
use CONSTANT_MOD, only : PI             ! if you need π
use NAMES_AND_TITLES_MOD, only : ncharline100   ! string buffers
use MY_ALLOC_MOD, only : my_alloc       ! memory allocation (common_source/tools)
use SPMD_MOD, only : SPMD_ALLREDUCE     ! MPI (engine/source/mpi)
```

See `.github/copilot-instructions.md` for the full coding standard.

## Related Documentation

- `common_source/README.md` — overview of all common_source components
- `.github/copilot-instructions.md` — which modules to use (and not use)
