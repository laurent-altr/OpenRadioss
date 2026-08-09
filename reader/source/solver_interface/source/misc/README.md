# Solver Interface — Miscellaneous (`reader/source/solver_interface/source/misc/`)

Entry points for LS-DYNA→Radioss conversion, parameter printing, library
versioning, and environment-variable setup.

## Key Files

| File | Role |
|------|------|
| `cpp_lsd2rad_convertor.cpp` | `cpp_read_dyna_and_convert_` — Fortran entry point: reads LS-DYNA deck into SDI then calls `SdiD2RConvert` to produce a Radioss SDI model |
| `buildmapping.cpp` | `BuildMapping()` — applies the LS-DYNA→Radioss conversion rule map via `sdiConvert::Convert` |
| `cpp_print_parameters.cpp` | Debug helper: print model parameters to stdout |
| `hm_lib_version.cpp` | Reports the HM reader library version string |
| `hm_reader_set_environment_variables.cpp` | Sets runtime environment variables required by the HM reader shared library |

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
- `reader/source/dyna2rad/dyna2rad/README.md` — `SdiD2RConvert` implementation
- `reader/source/solver_interface/source/includes/README.md` — `buildmapping.h` header
