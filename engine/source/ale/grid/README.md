# ALE Grid Rezoning (`engine/source/ale/grid/`)

Computes grid velocities for the ALE rezoning step — determines how to move the mesh between the pure Lagrangian position and the reference (typically smoothed) position.

## Key Files

| File | Role |
|------|------|
| `alelin.F` | Linear rezone: grid velocity is a linear interpolation between Lagrangian and reference |
| `alew.F` | Main grid velocity computation: weighted combination of smoothing strategies |
| `alew1.F` | Grid weight function 1 (spring smoothing) |
| `alew2.F` | Grid weight function 2 (Laplacian smoothing) |
| `alew4.F` | Grid weight function 4 |
| `alew5.F` | Grid weight function 5 |
| `alew6.F` | Grid weight function 6 |
| `alew7.F` | Grid weight function 7 |
| `alew8.F90` | Grid weight function 8 |
| `alewdx.F` | Grid velocity gradient computation |

## Rezoning Strategies

The ALE grid can follow different movement strategies:
- **Fully Lagrangian** (grid weight = 1): grid moves with material (no advection needed)
- **Fully Eulerian** (grid weight = 0): grid fixed (all motion is advected)
- **Arbitrary** (0 < weight < 1): intermediate — mesh smoothed to reduce distortion

`alew*.F` implement various smoothing algorithms that balance mesh quality against advection cost. The default (`alew.F`) uses a combination of spring and Laplacian smoothing.

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `engine/source/ale/ale2d/README.md` / `ale3d/README.md` — use grid velocities from here
