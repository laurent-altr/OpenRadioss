# Memory Allocation (`common_source/tools/memory/`)

The `MY_ALLOC` memory allocation wrapper — the standard allocation mechanism used throughout OpenRadioss.

## Key Files

| File | Role |
|------|------|
| `my_alloc.F90` | `MY_ALLOC` subroutine — allocates any Fortran array, checks status, calls `ARRET` on failure |
| `extend_array.F90` | Grow an existing array (realloc semantics: preserve contents, extend size) |
| `shrink_array.F90` | Shrink an array to a smaller size (reallocate and copy) |
| `array_reindex.F90` | Change array index bounds without changing content |

## MY_ALLOC Usage

**Always use `MY_ALLOC` instead of bare Fortran `ALLOCATE`.** This is a project-wide coding standard (see `.github/copilot-instructions.md`).

```fortran
use MY_ALLOC_MOD, only : my_alloc
real(kind=WP), allocatable :: arr(:)
call my_alloc(arr, n, 'arr in my_subroutine')
```

`MY_ALLOC` internally calls `ALLOCATE`, checks `STAT`, and calls `ARRET('allocation error: arr in my_subroutine')` if allocation fails. The third argument is the error message string — always make it descriptive enough to locate the failing site.

## Array Resizing

When an array may need to grow dynamically (e.g., contact pair list, neighbour list), use `extend_array.F90`:

```fortran
call extend_array(arr, new_size, 'arr in my_subroutine')
```

This allocates a new array, copies the old contents, and replaces the pointer. Old data is preserved.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `.github/copilot-instructions.md` — coding standard requiring `MY_ALLOC`
- `engine/source/system/README.md` — `ARRET` (fatal error handler called on allocation failure)
