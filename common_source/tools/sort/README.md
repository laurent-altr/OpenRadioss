# Sorting Algorithms (`common_source/tools/sort/`)

General-purpose sorting algorithms used throughout OpenRadioss for mesh operations, ID management, and data ordering.

## Key Files

| File | Role |
|------|------|
| `myqsort.F` | Integer quicksort — primary sort for node/element ID arrays |
| `myqsort_d.F90` | Double-precision floating-point quicksort |
| `myqsort_int.F` | Integer-only quicksort variant (no key-value pairs) |
| `quicksort.F` | Alternative quicksort with in-place partitioning |
| `triins.F` | Insertion sort — used for small arrays (< ~32 elements) where constant overhead dominates |
| `trirap.F` | Radix sort — O(n) sort for bounded-range integer arrays (e.g., element IDs within a known max) |
| `insertion_sort.F90` | Modern Fortran 90 insertion sort module |
| `icompc.F` | Integer comparison function for key-value pair sorts |
| `iparti.F` | Partition step extracted from quicksort (used independently for nth-element) |
| `echang.F` | Element exchange / swap utility |
| `remove_duplicates.F90` | Remove duplicate entries from a sorted integer array |
| `cppsort.cpp` | C++ `std::sort` wrapper — used when a C++ comparator is needed |
| `my_orders.c` | C-level index-sort: returns sorted indices without moving data |

## Algorithm Selection Guide

| Array size | ID range bounded? | Recommended |
|-----------|------------------|-------------|
| < 32 | — | `triins.F` (insertion sort) |
| 32 – 10⁵ | No | `myqsort.F` (quicksort, O(n log n)) |
| > 10⁵ | Yes | `trirap.F` (radix sort, O(n)) |
| Floating-point | — | `myqsort_d.F90` |

## Related Documentation

- `common_source/tools/README.md` — parent tools directory overview
- `common_source/tools/search/README.md` — binary search on sorted arrays
