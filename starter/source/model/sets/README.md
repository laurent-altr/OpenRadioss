# Set Definitions (`starter/source/model/sets/`)

Reads and resolves set definitions — the general group-of-entities system in OpenRadioss. Sets are the underlying mechanism for `GRNOD`, `GRSH`, `GRSO`, etc.

## Key Files (selected)

| File | Role |
|------|------|
| `set_init.F` | Initialise set data structures, allocate set tables |
| `clause_init.F` | Initialise clause (selection criterion) data |
| `set_operator.F` | Apply set boolean operations (union, intersection, difference, complement) |
| `set_merge_simple.F` | Merge two sets by simple union |
| `sort_sets.F` | Sort set member IDs for binary search |
| `fill_gr.F` | Fill group arrays from resolved set membership |
| `fill_gr_surf_ellipse.F` | Fill group for elliptical surface selection |
| `fill_gr_surf_plane.F90` | Fill group for planar surface selection |
| `fill_igr.F` | Fill inverse group (all entities not in set) |
| `check_eltyp.F` | Check element type compatibility in a set |
| `inverted_group_alloc/dealloc/init.F` | Manage inverted (complement) group storage |
| `hm_set.F` | HM binary reader for set definitions |

### Clause Creation Files

Each `create_*_clause.F` creates a selection clause for a specific entity type/geometry:

| File | Selects |
|------|---------|
| `create_node_all_clause.F` | All nodes |
| `create_node_box.F` | Nodes inside a `/BOX` |
| `create_node_from_element.F` | Nodes belonging to specified elements |
| `create_elem_all_clause.F` | All elements |
| `create_element_clause.F` | Elements by type/group |
| `create_surface_from_element.F` | Element surface faces |
| `create_rbody_clause.F` | Nodes belonging to a rigid body |
| `create_plane_clause.F90` | Entities on a half-plane |
| `create_ellipse_clause.F` | Entities inside an ellipsoid |

## Set Architecture

A set is defined by one or more **clauses** (selection criteria). Each clause selects entities by:
- Geometry (inside a box, on a surface, within a distance)
- Entity type (all nodes, all shells, etc.)
- Logical combination (AND, OR, NOT of other sets)

Clauses are evaluated at startup to build the final member list. The resolved sets are then used to define groups (`GRNOD`, `GRSH`, etc.).

## Related Documentation

- `starter/source/model/README.md` — parent model directory
- `starter/source/groups/README.md` — group construction from sets
