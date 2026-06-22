# Solver Interface — Nodes (`reader/source/solver_interface/source/nodes/`)

Fortran-callable wrappers that extract node coordinates and counts from
the SDI model into Fortran arrays for the starter kernel.

## Key Files

| File | Role |
|------|------|
| `cpp_nodes_read.cpp` | `cpp_nodes_read_` — bulk node read: fills `ITAB` (IDs), `X` (coordinates), `CMERGE` (coincident-merge flags), submodel and UID arrays |
| `cpp_node_read.cpp` (inside `cpp_nodes_read.cpp`) | `cpp_node_read_` — single-node read variant |
| `cpp_nodes_count.cpp` | `cpp_nodes_count_` — return total node count in the model |

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
