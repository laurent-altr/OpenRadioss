# MPI SPH Exchanges (`engine/source/mpi/sph/`)

Cross-domain communication for Smoothed Particle Hydrodynamics particle data.

## Key Files

| File | Role |
|------|------|
| `spmd_sphgat.F` | Gather SPH particle data from all domains |
| `spmd_sphgetv.F` | Get SPH particle velocities across domain boundaries |
| `spmd_sphvox.F` | Exchange SPH voxel cell data for neighbour search |
| `spmd_sptool.F` | SPH MPI utility tools |

## Description

SPH particles near domain boundaries require information from particles in neighbouring domains to compute kernel sums. `spmd_sphvox.F` exchanges the voxel grid data used for neighbour search across domain boundaries. `spmd_sphgat.F` gathers position and density data from ghost SPH particles for kernel summation. The exchange occurs each step because SPH particles move (unlike mesh nodes which only need ghost updates).

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/elements/sph/README.md` — SPH element computations
