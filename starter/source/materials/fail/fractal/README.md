# Fractal Damage (`starter/source/materials/fail/fractal/`)

Starter initialisation for /FAIL/FRACTAL: stochastic fractal damage model that introduces spatially correlated random scatter in material strength.

## Key Files

| File | Role |
|------|------|
| `hm_read_fractal_dmg.F90` | Parse /FAIL/FRACTAL card parameters |
| `fractal_dmg_init.F90` | Generate fractal damage field over element set |
| `fractal_element_neighbor.F90` | Build element neighbour graph for fractal correlation length |
| `fractal_elem_spmd_renum.F90` | Renumber elements across MPI domains for fractal field |
| `random_walk_dmg.F90` | Random walk algorithm for fractal field generation |

## Description

The fractal damage model generates a spatially correlated random field of initial damage `D₀(x)` over the mesh. The correlation length is set by the fractal dimension and element size. `fractal_element_neighbor.F90` builds the adjacency graph; `random_walk_dmg.F90` propagates random perturbations along the graph to produce the fractal spatial pattern. Used to model material inhomogeneity and scatter in failure.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/fractal/README.md` — runtime damage evaluation
