# Starter SPMD / Domain Decomposition

This subsystem performs the mesh partitioning that determines how the model is split across MPI ranks. The partitioning is done once in the starter and encoded in the restart file; the engine reads it and does not re-partition.

## Directory Structure

```
spmd/
├── domain_decomposition/   — Core partitioning algorithms
├── ddsplit/                — Domain split utilities
├── domdec1.F               — Domain decomposition driver (step 1: node/element assignment)
├── domdec2.F               — Domain decomposition driver (step 2: interface node identification)
├── igrsurf_split.F         — Surface group splitting across domains
├── deallocate_igrsurf_split.F — Cleanup of split surface data
├── globvars.F              — Global variable setup post-decomposition
├── get_size_tag.F          — Compute MPI tag sizes
├── cpp_reorder_elements.cpp — C++ element reordering for cache efficiency
└── cpp_split_tool.cpp      — C++ split domain utilities
```

## Domain Decomposition (`domain_decomposition/`)

| File | Role |
|------|------|
| `domdec1.F` | Assign elements to ranks (based on METIS partitioning) |
| `c_domain_decomposition.cpp` | C++ wrapper calling METIS graph partitioner |
| `c_domain_decomposition_rbody.cpp` | Special handling for rigid bodies (must stay on one rank) |
| `get_metis_arguments.F90` | Build METIS graph from mesh connectivity |
| `grid2mat.F`, `grid2m_wrap.cpp` | Map grid-based decomposition to material partitions |
| `domdec2.F` | Identify ghost nodes — nodes shared between adjacent ranks |
| `check_skew.F` | Validate skew frames at domain boundaries |
| `consider_edge.F` | Handle edge (1D element) partitioning |

## Partitioning Algorithm

The mesh is represented as a dual graph:
- **Nodes** of the graph = elements
- **Edges** = shared nodes between elements

METIS partitions this graph into `N` balanced sub-graphs (one per MPI rank), minimising the number of inter-partition edges (minimising communication volume). Rigid bodies are constrained to remain on a single rank.

After partitioning:
1. Each element and owned node is assigned a rank
2. Ghost nodes are identified (nodes that border another rank's elements)
3. Send/receive maps are built for ghost node communication
4. All this data is written into the restart file

## Surface Group Splitting

Contact and load surfaces (`/GRNOD`, `/GRSURF`) may span multiple ranks. `igrsurf_split.F` splits surface group definitions along domain boundaries so each rank only stores the portion of the surface it owns.

## HyperMesh Reader (`devtools/hm_reader/`)

The `devtools/` directory (adjacent to `spmd/`) contains an interface to the HyperMesh binary reader for importing mesh data in Altair's native HM format. This is a developer tool; production input uses the standard keyword deck.

## Related Documentation

- `engine/source/mpi/README.md` — how the engine uses the decomposition at runtime
- `starter/source/README.md` — overall starter pipeline
