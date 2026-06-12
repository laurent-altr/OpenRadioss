# tools/sort/

## Purpose
Sorting algorithms for arrays of various types (real, integer, double precision). Multiple implementations cover different size/type/portability trade-offs: recursive quicksort, hybrid quicksort/insertion, stack-based iterative quicksort, radix sort, and insertion sort.

## Files

| File | Description |
|------|-------------|
| `quicksort.F` | Subroutine `QUICKSORT` — recursive quicksort for float arrays; `FIRST`/`LAST` parameters sort a subrange |
| `trirap.F` | Subroutine `TRIRAP` — hybrid quicksort/insertion sort; uses insertion sort for partitions < 15 elements (better cache behaviour for small arrays) |
| `iparti.F` | Subroutine `IPARTI` — median-of-three pivot selection and partition helper used by `trirap` |
| `triins.F` | Subroutine `TRIINS` — insertion sort helper used by `trirap` for small partitions |
| `myqsort.F` | Subroutine `MYQSORT` — stack-based iterative quicksort for reals with permutation tracking; `STACKLEN`=128, threshold=9 |
| `myqsort_int.F` | Subroutine `MYQSORT_INT` — integer version of `myqsort` with permutation tracking |
| `myqsort_d.F90` | Module: stack-based iterative quicksort for double precision; Sedgewick algorithm |
| `insertion_sort.F90` | Module `INSERTION_SORT_MOD` — insertion sort with index generation for real and integer arrays; efficient for n < 1000 |
| `cppsort.cpp` | C++ `std::sort` wrappers for real/int key-value pair sorting, callable from Fortran |
| `echang.F` | Subroutine `ECHANG` — element swap utility used by sorting routines |
| `icompc.F` | Comparison function returning −1/0/1 for less/equal/greater; used as a sort comparator |
| `my_orders.c` | Radix sort for unsigned integers using 16-bit direct addressing; O(n) for 32-bit integers |
| `remove_duplicates.F90` | Module `REMOVE_DUPLICATES_MOD` — O(n) duplicate removal in sorted arrays with optional tolerance parameter |
| `array_reindex.F90` | Module `ARRAY_REINDEX_MOD` — reindexes real/integer arrays by applying a permutation index (output of sort-with-tracking routines) |

## Choosing an algorithm
| Need | Recommended routine |
|------|---------------------|
| Sort reals, keep permutation | `myqsort.F` |
| Sort integers, keep permutation | `myqsort_int.F` |
| Sort doubles, keep permutation | `myqsort_d.F90` |
| Fast sort, no permutation, real | `trirap.F` |
| Tiny arrays (n < 1000) | `insertion_sort.F90` |
| Large integer arrays, O(n) | `my_orders.c` |

## Dependencies
- Uses: `modules/precision_mod.F90` (Fortran files)
- Used by: domain decomposition, contact search, mesh renumbering throughout engine and starter
