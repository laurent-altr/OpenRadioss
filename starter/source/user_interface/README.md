# Starter User Interface (User Subroutines)

This subsystem handles user-defined extension points in the starter: custom material laws that require special initialisation, user-defined wall impact laws, and the dynamic library loading mechanism.

## Key Files

| File | Role |
|------|------|
| `dyn_userlib.c` | Dynamic library loader (same as engine version) |
| `dyn_userlib_callback.c` | Callback registration for starter user routines |
| `callback_c.c` | C-side callback dispatch for starter |
| `interface_utable.F` | Fortran dispatch table for user subroutines |
| `law_user.F` | User material law (LAW99) starter-side initialisation |

## User Material Initialisation (`law_user.F`)

When `/MAT/LAW99` (user material) is present, the starter calls the user library's `UMATINI` or equivalent entry point to allow the user code to:
- Validate parameters
- Compute derived quantities
- Set initial history variable values

These initialised values are packed into the PM/IPM arrays and written to the restart file.

## Dynamic Library Loading

The starter loads the user library at startup using the same `dyn_userlib.c` mechanism as the engine. This allows user-defined starter-side initialisation without modifying the OpenRadioss source.

## Related Documentation

- `engine/source/user_interface/README.md` — engine-side user subroutines (more complete)
- `starter/source/materials/README.md` — standard material law initialisation
