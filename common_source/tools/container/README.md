# Container Data Structures (`common_source/tools/container/`)

Generic container implementations: dynamic arrays and hash tables.

## Key Files

| File | Role |
|------|------|
| `IntVector.hpp` | C++ dynamic integer array (similar to `std::vector<int>`) with `push_back`, `resize`, `reserve` |
| `IntVector_api.cpp` | C-linkage API wrapping `IntVector` for use from Fortran via ISO C binding |
| `c_hash_table.cpp` | Open-addressing hash table with integer keys and integer values |
| `umap.cpp` | Unordered map (hash map) — C++ `std::unordered_map` wrapper |
| `umap_mod.F90` | Fortran 90 module exposing `umap` hash map operations via ISO C binding |

## Usage

### Dynamic Integer Array (IntVector)

Used when the number of elements is not known in advance. Fortran calls go through `IntVector_api.cpp`:

```fortran
use intVector_mod
type(c_ptr) :: vec
vec = intVector_create()
call intVector_push_back(vec, 42)
n = intVector_size(vec)
call intVector_destroy(vec)
```

### Hash Map (umap / c_hash_table)

Provides O(1) average key look-up for integer→integer mappings. Used for:
- Global-to-local node ID mapping in MPI domain decomposition
- Element ID look-up in connectivity tables

```fortran
use umap_mod
type(c_ptr) :: map
map = umap_create()
call umap_insert(map, key, value)
val = umap_find(map, key)
call umap_destroy(map)
```

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `common_source/tools/search/README.md` — binary search for sorted arrays (alternative for small maps)
