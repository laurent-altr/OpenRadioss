# Starter System Utilities

This subsystem provides the same low-level system utilities for the starter that `engine/source/system/` provides for the engine. Most files are identical or near-identical in function.

## Key Files

| File | Role |
|------|------|
| `arret.F` | Fatal error handler — print message and stop all MPI ranks |
| `error_alloc.F` | Memory allocation failure handler |
| `get_file_name_info.F` | Parse input file name and run number from command-line arguments |
| `fsdcod.F` | Decode file status codes for error reporting |
| `get_ibuiltin_arch.c` | C routine returning the build architecture identifier |

## Fatal Error Pattern

Identical to the engine pattern:

```fortran
call ARRET_MESSAGE('subroutine_name', 'description of problem')
call ARRET(2)
```

`ARRET` broadcasts the stop signal via MPI before exiting so no rank hangs.

## Architecture Detection (`get_ibuiltin_arch.c`)

Returns a numeric code identifying the compile-time architecture (linux64, win64, arm, etc.). Used in the `.out` file header and in version-compatibility checks for restart files.

## Related Documentation

- `engine/source/system/README.md` — engine-side equivalent (same pattern)
- `.github/copilot-instructions.md` — `MY_ALLOC` and `ARRET` usage rules
