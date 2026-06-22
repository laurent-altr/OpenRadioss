# User Interface (User Subroutines)

This subsystem provides the hook for user-defined extensions to OpenRadioss — custom material laws, forces, functions, and access routines loaded from a shared library at runtime.

## Purpose

The user interface allows simulation engineers to implement custom physics not covered by built-in capabilities, without modifying the core OpenRadioss source. User subroutines are compiled into a shared library (`user.so` on Linux, `user.dll` on Windows) and loaded at runtime.

## Key Files

| File | Role |
|------|------|
| `dyn_userlib.c` | Dynamic library loader — `dlopen`/`LoadLibrary` at startup |
| `dyn_userlib_callback.c` | Callback registration for user subroutines |
| `eng_callback_c.c` | C-side callback dispatch table |
| `interface_utable.F` | Fortran dispatch table — routes engine calls to user routines |
| `uaccess.F` | `UACCESS` routine — user access to internal model data arrays |
| `ufunc.F` | `UFUNC` routine — user-defined function (load curve replacement) |
| `suforc3.F` | `SUFORC` routine — user-defined external force contribution |
| `upidmid.F` | Map user part/material IDs to internal IDs |
| `nolib_usermat99.F` | Stub for `USERMAT99` when no user library is loaded |
| `uintbuf_mod.F` | Interface buffer module for user material access |
| `user_interface_mod.F90` | Main user interface Fortran module |

## User Subroutine Entry Points

| Subroutine | Description |
|-----------|-------------|
| `SIGEPS99` / `USERMAT99` | User-defined material law (LAW99) |
| `UFUNC` | User-defined load function (replaces `/FUNCT`) |
| `SUFORC` | User-defined external force on node group |
| `UACCESS` | Read/write access to internal arrays (positions, velocities, stresses) |
| `USERWI` | User-defined wall impact (rigid wall) law |

## Runtime Loading

At startup, `dyn_userlib.c` searches for `user.so` (or the path given by `-userlib` command-line argument). If found, it loads the library and registers the callback pointers. If not found, stub routines (`nolib_usermat99.F`, etc.) are used — they generate an error if called.

## `UACCESS` — Internal Data Access

`uaccess.F` exposes selected internal arrays to the user subroutine via a generic accessor interface. The user specifies an array name string (e.g. `'COORD'`, `'VELOC'`), and the routine returns a pointer to the corresponding engine array. This is the sanctioned mechanism for user code to read model state.

## Security Note

User subroutines execute with full access to the process memory. Only load user libraries from trusted sources, as they can modify any internal data.

## Related Documentation

- Root `HOWTO.md` — building with user subroutines
- `engine/source/materials/README.md` — built-in material laws (LAW1–LAW190)
- `engine/source/tools/README.md` — `ufunc.F` for function evaluation
