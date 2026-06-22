# ALE Grid Control (`starter/source/general_controls/ale_grid/`)

Reads the /ALE/GRID card that configures the mesh motion strategy for ALE simulations.

## Key Files

| File | Role |
|------|------|
| `hm_read_ale_grid.F` | Parse /ALE/GRID: rezoning method, Laplacian smoothing parameters, Lagrangian/Eulerian blend factor |
| `aleat.F` | Initialise ALE grid motion data (at-rest grid parameters) |

## Description

`hm_read_ale_grid.F` reads the `/ALE/GRID` block and sets the global ALE grid-motion parameters: the rezoning method (Laplacian, spring analogy, or prescribed), the number of smoothing iterations, and the velocity blending factor that interpolates between fully Lagrangian (`α=0`) and fully Eulerian (`α=1`) grid motion. `aleat.F` initialises the rest (reference) configuration coordinates used by the rezoning algorithm.

## Related Documentation

- `starter/source/general_controls/ale_cfd/README.md` — ALE advection scheme control
- `engine/source/ale/README.md` — ALE grid rezoning implementation
