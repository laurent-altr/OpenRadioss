# Initial Volume Fill (`starter/source/initial_conditions/inivol/`)

Reads /INIVOL to set the initial ALE/Euler multi-material volume fractions: determines which material occupies each ALE element at `t=0` by intersecting a surface mesh with the background grid.

## Key Files

| File | Role |
|------|------|
| `hm_read_inivol.F90` | Parse /INIVOL card: surface set, material IDs, fill direction |
| `init_inivol.F90` | Top-level initialisation: call surface-intersection and phase detection |
| `init_inivol_2D_polygons.F90` | 2D polygon intersection for structured ALE grids |
| `inifill.F` | Fill ALE elements inside the surface with the target material |
| `iniphase.F` | Assign phase (material) ID to each ALE element |
| `phase_detection.F` | Ray-casting inside/outside test against the surface |
| `phase_propagation.F` | Flood-fill to propagate phase assignment across element boundary |
| `getphase.F` | Query phase ID for a given element |
| `connesurf.F` | Build surface connectivity (edge/face adjacency) |
| `ale_box_creation.F` | Build axis-aligned bounding box for surface |
| `ale_box_coloration.F` | Colour elements by bounding-box overlap |
| `ale_element_size_computation.F` | Compute element characteristic size for surface sampling |
| `find_closest_node.F` | Locate nearest surface node to an ALE element centre |
| `in_out_side.F` | Signed distance test: element centre inside or outside surface |
| `inisoldist.F` | Compute signed distance field from surface to all elements |
| `ratio_fill.F` | Compute partial fill ratio for elements cut by the surface |
| `surface_min_max_computation.F` | Bounding box of surface geometry |
| `nfacette.F` | Count surface facets for each ALE element neighbourhood |
| `mean_node_norm2.F` | Average surface normals at nodes |

## Algorithm

1. Build bounding boxes and surface connectivity.
2. For each ALE element centre, perform a ray-casting inside/outside test against the triangulated surface (`phase_detection.F`).
3. Propagate phase labels to ambiguous elements by flood-fill (`phase_propagation.F`).
4. For elements cut by the surface, compute partial volume fractions (`ratio_fill.F`).
5. Write volume fractions to the restart file for ALE initialisation.

## Related Documentation

- `starter/source/initial_conditions/README.md` — parent directory
- `engine/source/ale/README.md` — ALE multi-material solver
- `starter/source/ale/README.md` — ALE grid and material setup
