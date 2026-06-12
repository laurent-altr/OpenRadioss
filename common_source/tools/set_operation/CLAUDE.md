# tools/set_operation/

## Purpose
Set operations on sorted integer arrays (union, intersection, difference, duplicate removal) and a dependency graph for sets. Used for computing shared nodes/elements between sets, domain decomposition, and resolving set evaluation order.

## Files

| File | Description |
|------|-------------|
| `set_tools.cpp` | C++ functions on sorted integer arrays: `remove_duplicates`, `union_2_sorted_sets`, `intersect_2_sorted_sets`, `difference_2_sorted_sets`, `count_member_list`; callable from Fortran |
| `set_graph.cpp` | C++ `set_graph` class: builds a dependency graph for sets, performs recursive dependency tracking, and detects circular dependencies |
| `set_surface_lines.cpp` | C++ surface/line element operations using tuples: union, delete, and intersection for 4-node surface segments and 2-node line segments; tracks element type and ID |

## Notes
- All input arrays must be sorted before calling `set_tools.cpp` routines.
- `set_graph` is used to determine the correct evaluation order for sets that reference other sets.

## Dependencies
- Used by: engine set management and domain decomposition routines
