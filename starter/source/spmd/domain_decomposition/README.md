# Domain Decomposition (`starter/source/spmd/domain_decomposition/`)

Performs the METIS-based mesh partitioning that assigns elements to MPI ranks.

## Key Files

| File | Role |
|------|------|
| `c_domain_decomposition.cpp` | C++ driver: build METIS input graph, call `METIS_PartMeshDual`, return partition array |
| `c_domain_decomposition_rbody.cpp` | Rigid body constraint: force all nodes of one rigid body onto the same rank |
| `domdec1.F` | Fortran top-level: prepare element/node data, call C++ METIS driver, post-process |
| `get_metis_arguments.F90` | Collect METIS control arguments from user settings (`/SPMD/METIS`) |
| `metis_mod.F90` | Fortran module wrapping METIS C API via ISO C binding |
| `grid2mat.F` | Map METIS graph output (element partition) to node partition |
| `grid2m_wrap.cpp` | C wrapper for `grid2mat.F` call |
| `initwg.F` | Initialise element weight for METIS (controls load balancing) |
| `initwg_shell.F` | Weights for shell elements |
| `initwg_solid.F` | Weights for solid elements |
| `initwg_beam.F` | (`initwg_poutre.F`) Weights for beam elements |
| `initwg_quad.F` | Weights for quad elements |
| `initwg_shell.F` | Weights for shell elements |
| `initwg_tri.F` | Weights for tri elements |
| `initwg_truss.F` | Weights for truss elements |
| `initwg_ressort.F` | Weights for spring elements |
| `initwg_x.F` | Weights for X-elements |
| `iwcontdd_type24.F` | Contact interface constraint for TYPE24 in METIS |
| `iwcontdd_type25.F` | Contact interface constraint for TYPE25 in METIS |
| `update_weight_inter_type2.F` | Update weights for TYPE2 (tied) interface |
| `update_weight_inter_type7.F` | Update weights for TYPE7 (contact) interface |
| `update_weight_inter_type11.F` | Update weights for TYPE11 interface |
| `update_weight_inter_type_24_25.F` | Update weights for TYPE24/25 |
| `update_weight_rbe3.F` | Weight update for RBE3 constraint nodes |
| `check_skew.F` | Validate skew frame assignments across domain boundaries |
| `consider_edge.F` | Consider/ignore graph edges (connectivity) for partitioning |
| `python_duplicate_nodes.F90` | Handle node duplication for Python-coupling boundaries |

## Domain Decomposition Steps

1. **Build graph** (`c_domain_decomposition.cpp`): construct element dual graph where elements are vertices and shared nodes are edges
2. **Assign weights** (`initwg*.F`): weight elements by estimated computational cost (shells heavier than solids for contact)
3. **Rigid body constraint** (`c_domain_decomposition_rbody.cpp`): coalesce all nodes of each rigid body to one rank
4. **METIS partition** (`METIS_PartMeshDual`): minimise inter-rank communication (cut edges) subject to load balance
5. **Node assignment** (`grid2mat.F`): from element partition, determine node partition (shared nodes go to one rank, ghost to others)
6. **Interface update** (`update_weight_inter_*.F`): adjust for contact interface topology

## Related Documentation

- `starter/source/spmd/README.md` — parent SPMD directory
- `engine/source/mpi/README.md` — engine MPI communication
