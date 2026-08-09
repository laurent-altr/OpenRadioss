# Graph Algorithms (`common_source/tools/graphs/`)

Graph data structure and algorithms used for mesh connectivity analysis, parallel colouring, and domain decomposition.

## Key Files

| File | Role |
|------|------|
| `Graph.hpp` | C++ `Graph` class: adjacency-list representation of a sparse graph |
| `Graph.cpp` | Implementation: BFS/DFS traversal, connected component labelling, graph colouring |
| `Graph_api.cpp` | C-linkage API for calling `Graph` methods from Fortran via ISO C binding |

## Algorithms Provided

### Connected Component Labelling
Finds isolated regions in the mesh graph (nodes connected to no elements, or separate sub-meshes). Used during starter model validation to warn about disconnected parts.

### Graph Colouring
Assigns colours to graph nodes such that no two adjacent nodes share the same colour. In OpenRadioss, elements are graph nodes and shared nodes define edges. Colour classes can then be processed in parallel by OpenMP without write conflicts — all elements of the same colour are independent.

```
Graph colouring → OpenMP colour-based assembly:
  for each colour c:
    !$OMP PARALLEL DO
    do i = 1, n_elems_of_colour_c
      call compute_force(elem(i))    ! no race: no shared nodes within colour
    end do
```

### Adjacency Construction
Builds node-to-element and element-to-element adjacency lists from element connectivity arrays. Used by METIS for domain decomposition input.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `engine/source/assembly/README.md` — OpenMP colouring usage in assembly
- `starter/source/spmd/README.md` — METIS-based domain decomposition
