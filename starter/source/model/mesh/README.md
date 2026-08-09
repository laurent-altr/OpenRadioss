# Mesh Quality (`starter/source/model/mesh/`)

Builds node-to-element connectivity tables needed for mesh quality checks and domain decomposition.

## Key Files

| File | Role |
|------|------|
| `build_cnel.F` | Build node-to-element connectivity (NTE table) — for each node, list of all containing elements |
| `build_cnel_sub.F` | Build NTE connectivity for a subset of elements |
| `build_addcnel_sub.F` | Extend NTE table after node merging (new connectivity for merged nodes) |

## Node-to-Element Connectivity

The NTE table enables O(1) access to all elements adjacent to any node. It is stored in CSR format:
- `CNEL(n)` — start index in `INEL` for node `n`'s element list
- `INEL(k)` — element index at position `k`

This table is constructed once after all elements have been read, and is used by:
1. Domain decomposition (METIS dual graph)
2. Contact broad-phase (which elements share a node with a candidate contact node?)
3. Result extrapolation (average element-centre stresses to nodes for output)

## Related Documentation

- `starter/source/model/README.md` — parent model directory  
- `starter/source/model/assembling/README.md` — assembling directory also builds connectivity
- `starter/source/spmd/README.md` — METIS domain decomposition uses NTE
