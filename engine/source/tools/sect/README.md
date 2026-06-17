# Engine Section Cuts (`engine/source/tools/sect/`)

Computes cross-section resultant forces and moments each output step.

## Key Files

| File | Role |
|------|------|
| `cutmain.F` | Master cross-section computation |

## Description

Section cuts (`/SECT`) compute the resultant force and moment on a planar cross-section through the model. Each output step: identify elements that cross the section plane, integrate the Cauchy stress over the cross-section area, and sum to get the resultant force vector and moment about the section centroid. Output goes to the TH file for load-path monitoring.

The section force computation requires traversing all elements (shells, solids, beams) that cross the cut plane, mapping their stresses to the cut-plane normal direction, and integrating. In MPI parallel runs `engine/source/mpi/sections/spmd_section.F` gathers contributions from all domains.

## Related Documentation

- `engine/source/tools/README.md` — parent tools directory
- `engine/source/mpi/sections/README.md` — MPI gather for sections
- `starter/source/tools/sect/README.md` — section definition
