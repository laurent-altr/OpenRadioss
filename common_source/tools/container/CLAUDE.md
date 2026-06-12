# tools/container/

## Purpose
C++ standard container wrappers (hash map and dynamic integer vector) with Fortran ISO_C_BINDING interfaces. Allows Fortran code to use high-performance C++ data structures without embedding C++ in Fortran source.

## Files

| File | Description |
|------|-------------|
| `umap.cpp` | C++ `std::unordered_map<int,int>` wrapper: `cpp_create_umap`, `cpp_free_umap`, `cpp_add_entry_umap`, `cpp_get_value_umap`, `cpp_reserve_umap` |
| `c_hash_table.cpp` | C/Fortran interface to multiple concurrent hash tables via a global `std::vector<std::unordered_map>`: `c_new_hash_`, `c_delete_hash_`, `c_hash_find_`, `c_hash_insert_` |
| `IntVector_api.cpp` | C/Fortran interface for `std::vector<int>`: create, delete, clear, push_back, size, copy, find operations |
| `IntVector.hpp` | C++ header for `IntVector` type (includes `<vector>`, `<algorithm>`, `<iterator>`) |
| `umap_mod.F90` | Module `UMAP_MOD` — Fortran wrapper for `umap.cpp` via `ISO_C_BINDING`; subroutines `add_entry`, `get_value`, `reserve_umap`; OpenMP critical sections for thread safety |

## Key Modules Exported
- **`UMAP_MOD`** — Fortran interface to C++ hash map with thread-safe access

## Notes
- OMP critical sections in `umap_mod.F90` protect hash map access from concurrent writes; remove if single-threaded performance is needed.

## Dependencies
- Used by: engine domain decomposition and connectivity routines needing fast key-value lookup
