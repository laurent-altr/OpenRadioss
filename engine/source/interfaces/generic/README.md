# Generic Interface Utilities (`engine/source/interfaces/generic/`)

Shared geometric and sorting utilities used by multiple contact interface types.

## Key Files

| File | Role |
|------|------|
| `check_coarse_grid.F` | Check coarse voxel grid quality (is voxel size appropriate?) |
| `inter_box_creation.F` | Create bounding boxes for contact segments |
| `inter_cell_color.F` | Assign cell colours for parallel contact processing |
| `inter_check_sort.F` | Validate contact sorting results |
| `inter_color_coarse_voxel.F` | Colour coarse voxel grid (for OpenMP independence) |
| `inter_color_voxel.F` | Colour fine voxel grid |
| `inter_component_bound.F90` | Component boundary detection for interfaces |
| `inter_count_node_curv.F` | Count nodes on curved contact surfaces |
| `inter_curv_computation.F` | Compute curvature of contact surface for segment normal computation |
| `inter_deallocate_wait.F` | Deallocate contact data structures at end of step |

## Shared Utilities

These routines are called from the per-type contact drivers (TYPE7, TYPE11, TYPE25, etc.) to:
1. Build bounding boxes for all contact segments (used by voxel search)
2. Compute surface normals (needed for contact normal force direction)
3. Assign colours to voxels/nodes so OpenMP parallelism is safe
4. Clean up temporary contact data at end of each step

## Related Documentation

- `engine/source/interfaces/intsort/README.md` — voxel-based broad phase
- `engine/source/interfaces/README.md` — parent interfaces directory
