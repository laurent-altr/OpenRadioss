# System Utilities Subsystem

This subsystem provides low-level system utilities used throughout the engine: error handling, file management, numerical utilities, and platform abstractions.

## Key Files

| File | Role |
|------|------|
| `arret.F` | Fatal error handler — prints error message and stops the run |
| `arret_message.F` | Formats and buffers error messages before calling `arret` |
| `error_alloc.F` | Handles memory allocation failure (called after failed `MY_ALLOC`) |
| `aleat.F` | Random number generator (Fortran interface) |
| `iniconstant.F` | Initialises physical and mathematical constants (calls `constant_mod`) |
| `machine.F` | Platform-specific machine epsilon and floating-point parameters |
| `mon_c.c` | C-level monitoring utilities (CPU time, memory usage) |
| `my_exit.c` | C-level clean exit routine used by `arret` |
| `my_isnan.F` | Portable `isnan()` wrapper for Fortran |
| `get_file_name_info.F` | Parse file name (base, extension, run number) from command-line arguments |

## Error Handling Pattern

All fatal errors in the engine follow this pattern:

```fortran
if (error_condition) then
  call ARRET_MESSAGE('module_name', 'description of error')
  call ARRET(2)  ! 2 = fatal error exit code
end if
```

`ARRET` ensures all MPI ranks exit together — it broadcasts the stop signal before calling `my_exit.c` so that no rank hangs waiting for a message that will never arrive.

## Memory Allocation

Memory allocation is always done through `MY_ALLOC` (defined in `common_source/modules/`), which:
1. Calls Fortran `ALLOCATE`
2. Checks the allocation status
3. Calls `error_alloc.F` on failure (which in turn calls `ARRET`)

Never use bare `ALLOCATE` without checking the status.

## Numerical Utilities

- `machine.F` — provides `XMACH` (machine epsilon) used for numerical tolerance checks
- `my_isnan.F` — portable NaN detection used in debug checks
- `aleat.F` — random number generator (used for noise initialisation, `/NOISE`)

## File Name Parsing (`get_file_name_info.F`)

OpenRadioss uses a structured file naming convention: `<basename>_0001` for the first restart. `get_file_name_info.F` extracts the base name and run number from the command-line argument so all subsystems open the correct files.

## Related Documentation

- `engine/source/README.md` — engine entry point (calls `iniconstant` at startup)
- `common_source/comm/README.md` — lower-level I/O and environment utilities
- `.github/copilot-instructions.md` — `MY_ALLOC` usage rules
