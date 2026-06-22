# SPH Element Initialisation (`starter/source/elements/sph/`)

Reads and initialises Smoothed Particle Hydrodynamics (SPH) elements (`/SPHCEL`).

## Key Files

| File | Role |
|------|------|
| `spinit3.F` | Main SPH initialisation: read connectivity, assign particles to smoothing cells |
| `spinih.F` | Initialise SPH smoothing length `h` for each particle |
| `spgrhead.F` / `spgrtails.F` | Write SPH element group header/tail to restart file |
| `sphonf0.F` | Initialise SPH neighbourhood (find initial neighbour lists within cutoff radius) |
| `sppart3.F` | SPH particle partitioning for MPI domain decomposition |
| `sporth3.F` | Compute initial SPH orthonormal basis for anisotropic kernels |
| `sptri.F` | Triangle-based SPH (surface SPH for impact/fragmentation) |
| `sptrivox.F` | Voxel-based neighbourhood search for SPH particles |
| `spclasv.F` | Classify SPH particles by type (bulk/surface/void) |
| `spbuc31.F` | SPH bucket neighbourhood search — O(n) neighbour finding |
| `nbsph.F` | Compute number of SPH neighbours within smoothing length |
| `inspcnd.F` | Impose initial conditions on SPH particles |
| `m24insph.F` | Convert TYPE24 brick cells to SPH particles (`/SOL2SPH`) |
| `soltosph.F` | Driver for solid-to-SPH conversion |
| `sphdcod.F` | Decode SPH element connectivity from group codes |

## SPH in OpenRadioss

SPH particles replace the mesh for large-deformation, fragmentation, and free-surface flows where mesh distortion would cause numerical failure. Each SPH particle is a Lagrangian point carrying mass, momentum, and internal state.

Key parameters set during initialisation:
- Smoothing length `h` (controls interpolation radius)
- Initial neighbourhood (particles within `κh` where κ is the kernel support radius)
- Mass (volume × density)

## Solid-to-SPH Conversion (`/SOL2SPH`)

Brick elements can be converted to SPH particles mid-run when a failure criterion is met. `m24insph.F` and `soltosph.F` prepare the mapping tables used by the engine to trigger this conversion.

## Related Documentation

- `starter/source/elements/README.md` — parent directory overview
- `starter/source/properties/sph/` — SPH property parameters (kernel type, smoothing)
- `engine/source/elements/README.md` — engine SPH force computation
