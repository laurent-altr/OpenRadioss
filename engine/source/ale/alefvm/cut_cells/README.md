# ALE Cut-Cell Coupling (`engine/source/ale/alefvm/cut_cells/`)

Handles ALE/FVM interface22 cut-cell geometry: computes partial cell volumes and fluxes for ALE cells intersected by a Lagrangian structure.

## Key Files

| File | Role |
|------|------|
| `a22conv3.F` | Convective flux at TYPE22 cut-cell faces (advection across interface) |
| `aflux3_int22_fvm.F` | ALE FVM flux at interface22 cut cells |
| `ale51_antidiff3_int22.F` | FCT anti-diffusion correction at TYPE22 cut-cell faces (ALE51 solver) |
| `ale51_upwind3_int22.F` | FCT upwind flux at TYPE22 cut-cell faces (ALE51 solver) |
| `eflux3_int22_fvm.F` | Energy flux at interface22 cut cells |

## Algorithm

When a Lagrangian structure cuts through an ALE/Euler cell:
1. The cut-cell geometry is computed in `engine/source/interfaces/int22/` (destroy_cell.F marks fully covered cells)
2. This directory provides the specialised flux computations for partially covered cells
3. `a22conv3.F` computes the fraction of the convective flux that passes through the uncovered (fluid-occupied) portion of the cell face
4. `ale51_*_int22.F` handle the FCT (Flux-Corrected Transport) variant used by the ALE51 solver for sharper interface capture

## Related Documentation

- `engine/source/ale/alefvm/README.md` — parent FVM-ALE directory
- `engine/source/interfaces/int22/README.md` — TYPE22 immersed boundary
