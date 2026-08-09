# Element Sandbox — Source (`tools/mockup/element_sandbox/source/`)

Fortran source implementing a single-group shell element computation loop,
extracted from `engine/source/elements/shell/`.  Exercises LAW1 and LAW2
material routines plus velocity/force integration for benchmarking and
compiler experiments.

## Key Files

| File | Role |
|------|------|
| `radioss_test.F` | Main driver: calls `INIT`, `INIT_DATA`, and the `GROUP_COMPUTATION` loop |
| `init.F` | `INIT` — allocates element and node arrays, sets geometry |
| `init_data.F` | `INIT_DATA` — fills material and integration-point data |
| `group_computation.F` | `GROUP_COMPUTATION` — outer loop over element groups; calls shell and material kernels |
| `shell_computation.F` | `SHELL_COMPUTATION` — QEPH/BT shell kinematics: strain, hourglass, internal forces |
| `law1_computation.F` | `LAW1_COMPUTATION` — elastic material (Young's modulus, Poisson) |
| `law2_computation.F` | `LAW2_COMPUTATION` — elastic-plastic material (isotropic hardening) |
| `m2cplr_computation.F` | Material coupler helper for two-component response |
| `update_acc.F` | `UPDATE_ACC` — nodal acceleration update from internal forces |
| `update_for.F` | `UPDATE_FOR` — nodal force assembly |

## Related Documentation

- `tools/mockup/element_sandbox/Readme.md` — build instructions and call tree (parent directory)
- `tools/mockup/element_sandbox/source/include/README.md` — include files
- `engine/source/elements/shell/README.md` — production shell elements
