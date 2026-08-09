# Engine-Specific Modules

This directory contains Fortran module definitions specific to the **engine** (not shared with the starter). These supplement the shared modules in `common_source/modules/`.

## Files

| File | Module | Purpose |
|------|--------|---------|
| `ams_work_mod.F90` | `AMS_WORK_MOD` | Working arrays for the AMS/SMS solver (allocated once, reused each step) |
| `dt_mod.F` | `DT_MOD` | Current time step `dt`, global time `t`, and DT history |
| `file_descriptor_mod.F90` | `FILE_DESCRIPTOR_MOD` | Engine file unit numbers (output files, restart, etc.) |
| `user_interface_mod.F90` | `USER_INTERFACE_MOD` | User subroutine callback registration and dispatch table |

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `interfaces/` | Engine-specific interface (contact) module definitions — data arrays not in `common_source/interf/` |

## Key Modules

### `DT_MOD`
Provides the global time step `dt` and current time `t` accessible throughout the engine without passing them as arguments. Updated at the start of each time loop iteration.

### `AMS_WORK_MOD`
Pre-allocates the large working arrays used by the SMS solver to avoid repeated allocation overhead in the time loop.

### `USER_INTERFACE_MOD`
Holds the callback function pointers loaded from the user library at startup. Enables any engine routine to call user subroutines without direct linking.

## Related Documentation

- `common_source/modules/README.md` — the much larger set of shared modules (use these first)
- `engine/source/ams/README.md` — AMS subsystem using `ams_work_mod`
- `engine/source/user_interface/README.md` — user subroutine infrastructure
