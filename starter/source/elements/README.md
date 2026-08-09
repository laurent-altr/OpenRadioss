# Starter Elements Subsystem

This subsystem reads element connectivity and property data, validates element geometry, and initialises element data structures for the restart file.

## Role

The starter's `elements/` directory handles **input parsing and geometric initialisation** for all element types. The engine's `engine/source/elements/` computes forces; this subsystem builds the data structures those computations require.

## Directory Structure

```
elements/
├── beam/          — Beam element readers
├── elbuf_init/    — Element buffer initialisation (intbuf setup)
├── ige3d/         — IGE3D element initialisation
├── initia/        — Element initial state (stress, strain from INISTA/INIMAP)
├── joint/         — Joint element readers
├── nodes/         — Node data management (merging, reconnection)
├── reader/        — HyperMesh-format element readers
├── sh3n/          — Triangular shell element initialisation
├── create_element_group.F90 — Build element group data structures
└── get_element_group.F90    — Retrieve element group by type/ID
```

## Key Operations

### Geometric Initialisation
For each element type, the starter computes:
- Initial element normals and reference frames
- Characteristic element length (used in DT estimation and output)
- Element volume / area / thickness
- Jacobian check (flag zero/negative volume elements)

### Node Merging (`nodes/`)
When multiple nodes are geometrically coincident (typically from CAD import), the starter can automatically merge them:
- `merge_node.F` — identify node pairs within a tolerance
- `merge_bucket_search.F` — spatial hash search for merge candidates
- `auto_node_merge.F` — automatic merge with connectivity update
- `reconnect.F` — update element connectivity after merge

### HyperMesh Reader (`reader/`)
Reads geometry directly from HyperMesh binary format (`hm_read_*.F`), bypassing the text keyword deck. Used in tightly integrated Altair-workflow pipelines.

### Element Buffer Init (`elbuf_init/`)
Initialises the element buffer (`intbuf_struct`) that the engine uses to track element-local integer metadata (integration point status, failure flags, history variable indices).

### Initial State (`initia/`)
Applies `/INISTA` and `/INIMAP` data to set pre-existing stress/strain in elements before the restart is written.

## Related Documentation

- `engine/source/elements/README.md` — runtime element force computation
- `starter/source/materials/README.md` — material data that pairs with elements
- `starter/source/properties/README.md` — property/section data for elements
