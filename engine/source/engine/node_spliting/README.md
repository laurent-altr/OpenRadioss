# Node Splitting (`engine/source/engine/node_spliting/`)

Implements node splitting for adaptive element refinement — creates new nodes at element edge midpoints during remeshing.

## Key Files

| File | Role |
|------|------|
| `detach_node.F90` | Detach a node from its current element: break shared node into two independent nodes (for crack opening or material separation) |
| `ghost_shells.F90` | Create ghost shell elements adjacent to split nodes (maintain mesh topology) |
| `ghost_shells.cpp` | C++ implementation of ghost shell topology update |
| `update_pon.F90` | Update point-of-nucleation (PON) data after node splitting |

## Node Splitting Use Case

Node splitting is used for:
1. **Adaptive refinement**: split an element by creating a new node at edge midpoint, connecting it to form two child elements
2. **Crack propagation**: split a node so that crack faces can open independently (alternative to XFEM level-set approach — element-deletion + node-splitting method)

The note: `node_spliting` is the intentional spelling in the codebase — do not rename to `node_splitting`.

## Related Documentation

- `engine/source/engine/README.md` — parent engine directory
- `engine/source/model/README.md` — adaptive mesh refinement (`admdiv.F`)
- `engine/source/elements/xfem/README.md` — XFEM crack propagation (alternative approach)
