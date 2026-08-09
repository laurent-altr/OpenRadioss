# Common Communication Utilities (common_source/comm)

This directory provides low-level I/O and system utilities shared between the starter and engine. These are below the level of the physics subsystems — they handle compressed arrays, memory reporting, and system interaction.

## Files

| File | Role |
|------|------|
| `compress_nnz.F` | Compress sparse arrays using non-zero pattern (CSR/CSC format) |
| `decompress_nnz.F` | Decompress sparse arrays from compressed format |
| `read_array.F` | Read a Fortran array from binary file (with optional byte-swap) |
| `write_array.F` | Write a Fortran array to binary file |
| `write_mod.F` | Buffered write module — accumulates output before flushing |
| `write_units.F` | Map logical unit numbers to file paths |
| `memory_use_c.c` | C routine to query process memory usage (RSS, virtual) |
| `radioss_set_env_variable.c` | Set environment variables from Fortran (portable wrapper) |
| `stacksize.cpp` | C++ utility to query and set the stack size limit |

## Sparse Array Utilities (`compress_nnz.F`, `decompress_nnz.F`)

These routines convert between dense and Compressed Sparse Row (CSR) representations. Used by the implicit solver and AMS subsystems to efficiently store the sparse stiffness and mass matrices.

```
Dense array  →  compress_nnz  →  (values[], col_idx[], row_ptr[])  (CSR)
                decompress_nnz  ←
```

## Binary Array I/O (`read_array.F`, `write_array.F`)

Portable binary array read/write used throughout the restart file system. Handles:
- Byte-ordering (big-endian / little-endian detection)
- Mixed-precision reads (read double, store as single)
- Record-length markers compatible with Fortran unformatted I/O

## Memory Monitoring (`memory_use_c.c`)

Reports current process memory usage by reading `/proc/self/status` on Linux. Used to print memory consumption at key checkpoints in the `.out` file.

## Stack Size (`stacksize.cpp`)

On Linux, sets the stack size limit via `setrlimit`. Large automatic arrays in Fortran can overflow the default stack; this utility raises the limit if the OS allows it.

## Environment Variables (`radioss_set_env_variable.c`)

Wraps `setenv()` / `putenv()` to allow Fortran code to set environment variables at runtime. Used for configuring MPI and external library behaviour.

## Related Documentation

- `common_source/README.md` — overview of all common_source components
- `engine/source/output/restart/` — uses `write_array.F` for restart writing
- `engine/source/implicit/README.md` — uses sparse array utilities
