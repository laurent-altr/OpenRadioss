# modules/

## Purpose
Core Fortran module definitions shared across all of OpenRadioss. This is the most-referenced directory in `common_source`; virtually every other module depends on at least `precision_mod.F90`. Organized by theme below. Subdirectories hold domain-specific modules (loads, elements, constraints, etc.).

## Files

### Precision & numeric utilities
| File | Module | Description |
|------|--------|-------------|
| `precision_mod.F90` | `PRECISION_MOD` | Defines `WP` (working precision kind parameter); the single source of truth for floating-point precision |
| `constant_mod.F` | `CONSTANT_MOD` | Mathematical constants: `zero`, `half`, `one`, `two`, `three`, `pi`, etc. |
| `cast_mod.F90` | `CAST_MOD` | `double_to_my_real()` — safely casts double-precision values to `WP` with NaN/overflow protection |
| `root_finding_algo_mod.F90` | `ROOT_FINDING_ALGO_MOD` | `brent_algo()` — Brent's method root finder with configurable tolerance |

### Model entity definitions
| File | Module | Description |
|------|--------|-------------|
| `groupdef_mod.F` | `GROUPDEF_MOD` | Types for element groups, surface groups, line groups, and subset definitions |
| `setdef_mod.F` | `SETDEF_MOD` | Set and set-collection structures for nodes, elements, parts, rigid bodies, and segments |
| `optiondef_mod.F` | `OPTIONDEF_MOD` | Analysis option types and parameters used during model setup |
| `names_and_titles_mod.F` | `NAMES_AND_TITLES_MOD` | String identifiers and descriptions for model entities |
| `unitab_mod.F` | `UNITAB_MOD` | Unit table: definitions and conversion factors for all physical units used in I/O |

### Mesh & connectivity
| File | Module | Description |
|------|--------|-------------|
| `array_mod.F` | `ARRAY_MOD` | Dynamic `array_type` for managing 1D/2D/3D integer and real arrays |
| `connectivity.F90` | `CONNECTIVITY_MOD` | Shell element connectivity: `shell_` type (nodes, material IDs, PIDs, user IDs, damage); C interop for local-to-global mapping |
| `nodal_arrays.F90` | `NODAL_ARRAYS_MOD` | Nodal state arrays (acceleration, velocity, displacement) used throughout engine calculations |
| `skew_mod.F90` | `SKEW_MOD` | `skew_` type for local coordinate systems (skew arrays + processor communication lists for MPI) |
| `element_user_id.F90` | `ELEMENT_USER_ID_MOD` | `id_limits_` type; `element_user_id()` returns user IDs and group IDs for all element types |

### Physics & simulation models
| File | Module | Description |
|------|--------|-------------|
| `spring_functions_mod.F90` | `SPRING_FUNCTIONS_MOD` | `spring_functions_type` — hardening type, function type, and table data for spring elements |
| `failwave_mod.F` | `FAILWAVE_MOD` | `failwave_str_` — failure front tracking (nodal status, levels, stacks) for wave-propagating failure |
| `sensor_mod.F90` | `SENSOR_MOD` | Sensor definition and state tracking for simulation control triggers |
| `inivel_mod.F90` | `INIVEL_MOD` | Initial velocity types: `general_inivel_`, `axis_inivel_`, `fvm_inivel_`; sensor/time-based triggering |
| `xfem2def_mod.F` | `XFEM2DEF_MOD` | XFEM crack-front structures: `xfem_shell_`, `xfem_phantom_`, `xfem_lvset_`, `xfem_edge_` |
| `pinchtype_mod.F` | `PINCHTYPE_MOD` | `pinch` type for tracking pinched nodes in shell elements (displacement/force data) |
| `seatbelt_mod.F` | `SEATBELT_MOD` | Seatbelt structures: slipring frames, guided cables, retractors, and interaction data |
| `nlocal_reg_mod.F` | `NLOCAL_REG_MOD` | Non-local damage data for elements using non-local regularization failure criteria |
| `multimat_param_mod.F90` | `MULTIMAT_PARAM_MOD` | Material, EOS, failure, and viscosity parameters for multi-material ALE/CFD elements |
| `table4d_mod.F` | `TABLE4D_MOD` | `table_4d_` type for 4-dimensional tabular data (X-axis + 1D–4D Y arrays) for material properties |

### Failure statistics
| File | Module | Description |
|------|--------|-------------|
| `random_walk_def_mod.F90` | `RANDOM_WALK_DEF_MOD` | `random_walk_`, `fractal_`, `fail_fractal_` — fractal damage initialization using random walks |
| `brokmann_random_def_mod.F90` | `BROKMANN_RANDOM_DEF_MOD` | `brokmann_elem_`, `brokmann_`, `fail_brokmann_` — windshield failure modeling with Brokmann random coefficients |

### Parallel / HPC
| File | Module | Description |
|------|--------|-------------|
| `parith_on_mod.F90` | `PARITH_ON_MOD` | `element_pon_` and `interface_pon_` types — skyline force/mass arrays and processor communication arrays for parallel assembly |
| `mds_rest.mod.F` | `MDS_REST_MOD` | MDS (Material Data Structure) restart: material IDs, dependent-variable labels, and file paths for restart I/O |

### I/O & output control
| File | Module | Description |
|------|--------|-------------|
| `inoutfile_mod.F` | `INOUTFILE_MOD` | File I/O management: output formats (animation, restart, stats, H3D) and file unit management |
| `state_mod.F` | `STATE_MOD` | `dynain_database` type for dynain (dynamic input) data including timestep info and element counts |
| `check_mod.F` | `CHECK_MOD` | Restart file control flags and check-message arrays for `/CHECK` option diagnostics |
| `aleanim_mod.F` | `ALEANIM_MOD` | `fani_cell_` type for ALE cell animation data (F18 flow, vorticity) used in output generation |
| `outmax_mod.F` | `OUTMAX_MOD` | Tracks maximum values (displacement, velocity, stress, strain) across elements and nodes for output reporting |
| `plot_curve_mod.F` | `PLOT_CURVE_MOD` | Interface declaration for `plot_curve()` ASCII curve-plotting subroutine (implementation in `sortie/`) |

### Python integration
| File | Description |
|------|-------------|
| `python_mod.F90` | Module `PYTHON_MOD` — Fortran interface to Python functions for loads, sensors, element updates; manages interpreter init and function dispatch via C++ |
| `python_element_mod.F90` | Module `PYTHON_ELEMENT_MOD` — types `python_element_keyword`, `python_element_local`, `python_element_global` for Python-controlled element data |
| `cpp_python_funct.cpp` | C++ implementation: dynamic Python library loader, function dictionaries, node/element variable synchronization between Fortran and Python |
| `cpp_python_funct.h` | C++ header: Python function pointer types, `KeywordPairs` type for Fortran↔Python variable mapping |
| `python_signal.h` | Signal interception utilities for `SIGINT` handling during Python execution |
| `cpp_python_sampling.h` | Logarithmic/linear curve-data sampling and safe double-to-double conversion helpers |

## Key Modules Exported (most widely used)
- **`PRECISION_MOD`** — `WP` kind parameter (used everywhere)
- **`CONSTANT_MOD`** — physical/mathematical constants
- **`UNITAB_MOD`** — unit conversion table
- **`GROUPDEF_MOD`**, **`SETDEF_MOD`** — model entity containers

## Sub-directories
| Sub-directory | Contents |
|---------------|---------|
| `airbag/` | FVM airbag mesh control data |
| `ale/` | ALE method data structures |
| `boundary_conditions/` | Eulerian and general BC types |
| `constraints/` | RBE3 and rigid wall types |
| `elements/` | General element and SFEM types |
| `interfaces/` | Contact interface buffer/type definitions |
| `loads/` | Load type definitions and domain decomposition |
| `mat_elem/` | Material and element parameter types |
| `output/` | Animation, state file, time history, checksum types |
