# Model Assembly (`starter/source/model/assembling/`)

Assembles global connectivity data structures after all elements have been read. Creates node-to-element adjacency needed for contact detection, domain decomposition, and output.

## Key Files

| File | Role |
|------|------|
| `hm_read_part.F` | HM binary reader for `/PART` definitions (connects elements to property/material) |
| `hm_read_submodel.F` | HM reader for submodel (`/SUBMODEL`) references |
| `hm_read_subset.F` | HM reader for subset definitions |
| `build_cnel.F` | Build connectivity: for each node, list of elements that reference it |
| `build_cnel_sub.F` | Build connectivity for a sub-range of nodes |
| `build_addcnel_sub.F` | Build additional connectivity (added nodes from merging) |

## Node-to-Element Connectivity

`build_cnel.F` constructs the node-to-element (NTE) table: for each node `n`, the list of all elements that contain `n`. This is used for:
- Contact sorting: find all elements adjacent to a candidate contact node
- Domain decomposition: METIS needs the dual graph (element adjacency through shared nodes)
- Output: extrapolate element-centre results to nodes by averaging adjacent elements

The connectivity is stored in a CSR (Compressed Sparse Row) format and written to the restart file.

## Related Documentation

- `starter/source/model/README.md` — parent model directory
- `starter/source/spmd/README.md` — domain decomposition uses this connectivity
