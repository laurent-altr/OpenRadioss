# Search Algorithms (`common_source/tools/search/`)

Binary search routines for sorted arrays of integers and floating-point values.

## Key Files

| File | Role |
|------|------|
| `dichotomic_search_i_asc.F` | Binary search on ascending integer array — returns index of target or insertion point |
| `dichotomic_search_i_desc.F` | Binary search on descending integer array |
| `dichotomic_search_r_asc.F` | Binary search on ascending real array |
| `dichotomic_search_r_desc.F` | Binary search on descending real array |

## Usage Pattern

```fortran
use search_mod, only : dichotomic_search_i_asc
integer :: idx
call dichotomic_search_i_asc(array, n, target, idx)
! idx: position where target was found, or -(insertion_point) if not found
```

All routines return the found index (1-based) if the target is present, or a negative value indicating the insertion point if not found. The calling code uses this convention to distinguish "found" from "not found" without a separate boolean.

## Usage in OpenRadioss

Binary search is used wherever a sorted array needs O(log n) look-up:
- Finding element IDs in a sorted group list
- Locating a node ID in a sorted connectivity array
- Bracket search for 1D table interpolation (finding the enclosing knot interval)

For O(1) average look-up (when `n` is large), use the hash table in `common_source/tools/container/`.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `common_source/tools/sort/README.md` — sorting routines (input must be sorted first)
- `common_source/tools/container/README.md` — hash table for O(1) look-up
