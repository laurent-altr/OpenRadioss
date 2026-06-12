# engine/source/multifluid/

## Purpose
Multi-material finite-volume method (FVM) for ALE elements containing multiple
fluid materials in the same cell. Handles material mixing, interface tracking,
volume/flux computation, and boundary conditions for multi-fluid ALE simulations.

## Key files

| File | Role |
|------|------|
| `centroid.F` | Computes cell centroids for multi-material FVM |
| `connectivity.F` | Builds cell-face connectivity for the FVM mesh |
| `multi_allocate.F` | Allocates multi-material working arrays |
| `multi_deallocate.F` | Deallocates multi-material working arrays |
| `multi_bilan.F` | Multi-material mass balance check |
| `multi_buf2var.F` | Copies multi-material buffer to element variable arrays |
| `multi_compute_dt.F` | Time-step estimate for multi-material cells |
| `multi_computevolume.F` | Computes partial volumes occupied by each material |
| `multi_ebcs.F`, `multi_inlet_ebcs.F`, `multi_nrf_ebcs.F` | Multi-material Eulerian boundary conditions |
| `multi_evolve_global.F` | Global (cell-averaged) multi-material state update |
| `multi_evolve_partial.F` | Per-material partial-volume evolution |
| `multi_face_data_elem.F` | Computes face-centred data for flux computation |
| `multi_fluxes_computation.F` | Computes advective fluxes between cells |
| `multi_fvm2fem.F` | Maps FVM cell quantities to FEM nodes |
| `multi_globalize.F` | Gathers multi-material partial quantities into cell average |
| `multi_muscl_compute_pressure.F90` | MUSCL higher-order pressure reconstruction |
| `multi_muscl_fluxes_computation.F` | MUSCL flux computation for multi-fluid |
| `multi_muscl_gradients.F` | MUSCL gradient computation |

## Relationship to ALE and fluid
- `multifluid/` handles the **multi-material ALE** case: one FVM cell with N materials.
- Single-material ALE advection is in `engine/source/ale/alefvm/`.
- Pure Eulerian (single material) is in `engine/source/fluid/`.
- All three paths are coordinated by `ALEMAIN` in `engine/source/ale/alemain.F`.

## Dependencies
- Called by: `ALEMAIN` → `RESOL`
- Uses: `multimat_param_mod.F90` from `common_source/modules/`
- MPI: `engine/source/mpi/fluid/spmd_cfd.F`
