# Set Operations (`common_source/tools/set_operation/`)

C++ utilities for building and manipulating mesh entity sets (node groups, surface sets, contact surfaces).

## Key Files

| File | Role |
|------|------|
| `set_tools.cpp` | General set operations: union, intersection, difference on sorted integer arrays |
| `set_graph.cpp` | Build a connectivity graph from a set of surface triangles/quads (used for surface group construction) |
| `set_surface_lines.cpp` | Extract boundary line segments from a surface set (perimeter detection) |

## Usage in OpenRadioss

These utilities are called during starter model assembly when building groups:

- `set_tools.cpp` — combines group definitions from multiple `GRSH`, `GRNOD` definitions (union/intersection modes)
- `set_graph.cpp` — builds the adjacency graph for a contact surface to detect internal vs boundary edges
- `set_surface_lines.cpp` — finds perimeter edges of a contact surface for contact edge detection

All operations work on sorted integer arrays (node/element IDs). The Fortran side calls these C++ routines via `extern "C"` linkage.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `engine/source/groups/README.md` — group management in engine
- `starter/source/groups/README.md` — group construction in starter
