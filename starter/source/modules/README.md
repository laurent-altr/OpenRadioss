# Starter Modules

This directory contains Fortran module definitions specific to the **starter** (not shared with the engine). These supplement the shared modules in `common_source/modules/`.

## Files

| File | Module | Purpose |
|------|--------|---------|
| `constaint_mod.F90` | `CONSTAINT_MOD` | Starter-side constraint tracking (note: intentional spelling) |
| `defaults_mod.F90` | `DEFAULTS_MOD` | Default parameter values for all optional keywords |
| `elm_group_mod.F90` | `ELM_GROUP_MOD` | Element group data for starter-specific group operations |
| `file_descriptor_mod.F90` | `FILE_DESCRIPTOR_MOD` | File unit number management (starter file set) |
| `spmd_mod.F90` | `SPMD_MOD` | Starter-side SPMD wrappers (subset of engine SPMD_MOD) |

## Notes

- `defaults_mod.F90` is particularly important: it defines the default value for every optional parameter in the entire keyword deck. If adding a new keyword, add its default value here.
- `spmd_mod.F90` in the starter is a simplified version used only for the domain decomposition step. For full MPI functionality, the engine's version in `engine/source/mpi/` provides the complete API.
- `constaint_mod.F90` (note: "constaint" without the 'r' is the historical spelling in this codebase — do not rename).

## Related Documentation

- `common_source/modules/README.md` — the much larger set of shared modules
- `starter/source/README.md` — overall starter architecture
