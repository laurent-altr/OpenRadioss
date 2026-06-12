# tools/memory/

## Purpose
Array memory management wrappers with error handling and data preservation. Provides a consistent allocation interface used throughout the solver so that out-of-memory conditions are handled uniformly.

## Files

| File | Module | Description |
|------|--------|-------------|
| `my_alloc.F90` | `MY_ALLOC_MOD` | Primary allocation interface for all data types and structures; checks allocation status and aborts with a diagnostic message on failure; called by nearly every module that uses dynamic memory |
| `extend_array.F90` | `EXTEND_ARRAY_MOD` | Generic interfaces to extend/reallocate integer, real, and double-precision arrays (1D–3D), preserving existing data via copy |
| `shrink_array.F90` | `SHRINK_ARRAY_MOD` | Generic interfaces to shrink integer, real, and double-precision 1D arrays to a smaller size while preserving values |

## Key Modules Exported
- **`MY_ALLOC_MOD`** — the canonical allocation entry point for the whole codebase
- **`EXTEND_ARRAY_MOD`** — safe array growth with data copy
- **`SHRINK_ARRAY_MOD`** — safe array reduction

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: virtually all modules that allocate dynamic arrays
