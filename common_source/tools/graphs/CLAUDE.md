# tools/graphs/

## Purpose
Graph algorithms (path finding and cycle detection via depth-first search) used in domain decomposition and mesh connectivity analysis.

## Files

| File | Description |
|------|-------------|
| `Graph.hpp` | C++ `Graph` class header: adjacency list structure, `exp_graph` export structure for results |
| `Graph.cpp` | `Graph` implementation: `build_path()` finds connected components via DFS, `build_cycle()` detects cycles, `dfs()` recursive depth-first traversal |
| `Graph_api.cpp` | Fortran/C API: `graph_build_path_`, `graph_get_sizes_`, `graph_get_path_`, `graph_free_memory_`, `graph_build_cycles_`, `graph_get_nb_adj_`, `graph_get_adj_` |

## Notes
- All public entry points are in `Graph_api.cpp` with trailing-underscore Fortran calling conventions.

## Dependencies
- Used by: engine domain decomposition, set dependency resolution, and mesh connectivity routines
