# LAW131 — Return Mapping Solvers (`engine/source/materials/mat/mat131/return_mapping/`)

Modular return-mapping algorithms for the LAW131 framework; three solvers
available for stability / performance trade-offs.

## Key Files

| File | Role |
|------|------|
| `cppm_solids.F90` / `cppm_shells.F90` | Closest-Point Projection Method (CPPM): robust Newton-based return |
| `cutting_plane_solids.F90` / `cutting_plane_shells.F90` | Cutting-plane algorithm: explicit iterative return |
| `nice_solids.F90` / `nice_shells.F90` | NICE (vectorised explicit): fastest, suited for explicit time integration |

## Related Documentation

- `engine/source/materials/mat/mat131/README.md` — parent directory
