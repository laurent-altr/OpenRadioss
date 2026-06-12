# comm/

## Purpose
MPI-parallel I/O utilities, array compression/decompression for restart buffers, memory reporting, OpenMP stack size management, and environment variable helpers. Used by both the starter and engine builds.

## Files

| File | Description |
|------|-------------|
| `write_mod.F` | Module `WRITE_MOD` — Fortran wrappers for writing integers/arrays; prevents dummy-argument mismatches by delegating to C routines in `tools/input_output/write_routines.c` |
| `write_array.F` | Subroutine `WRITE_ARRAY` — compresses an integer array's nonzero elements into `TABVINT` compact form for restart storage |
| `read_array.F` | Subroutine `READ_ARRAY` — decompresses `TABVINT` back into a full integer array |
| `compress_nnz.F` | Subroutine `COMPRESS_C_NNZ` — run-length-style compression of character arrays (removes spaces, stores position/count pairs) |
| `decompress_nnz.F` | Subroutines `DECOMPRESS_I_NNZ` / `DECOMPRESS_R_NNZ` — restore integer or real arrays from compressed form |
| `write_units.F` | Subroutine `WRITE_UNITS` — writes the unit table (mass/length/time conversion factors) to binary output via `UNITAB_MOD` |
| `memory_use_c.c` | Function `map_memory` — reads `/proc/<pid>/status` to retrieve VM peak, size, RSS, HWM, and stack usage (Linux only) |
| `radioss_set_env_variable.c` | Sets the `TMPDIR` environment variable; provides multiple Fortran calling-convention variants for Windows and Linux |
| `stacksize.cpp` | Function `solver_stacksize` — queries/configures OpenMP thread stack size via `OMP_STACKSIZE` / `KMP_STACKSIZE`; Intel and GFortran compiler support |

## Key Modules Exported
- **`WRITE_MOD`** (`write_mod.F`) — `WRITE_INTEGER`, `WRITE_INTEGER_1D`, `WRITE_INTEGER_2D`

## Dependencies
- Uses: `tools/input_output/write_routines.c` (low-level C writes), `modules/unitab_mod.F` (unit definitions)
- Used by: engine and starter output/restart routines throughout both source trees
