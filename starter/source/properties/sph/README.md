# SPH Property (`starter/source/properties/sph/`)

Reads Smoothed Particle Hydrodynamics particle properties (`/PROP/SPH`).

## Key Files

| File | Role |
|------|------|
| `hm_read_prop02.F` | Read SPH property: smoothing length `h`, kernel type, particle mass |

## Description

The SPH property card defines the SPH discretisation parameters:
- Initial smoothing length `h` (or number of neighbours to target)
- Kernel function type (cubic spline, quintic, etc.)
- Adaptive `h` option: whether `h` varies with local particle density
- Particle mass (may be read from nodal data or set here)

SPH is used for large-deformation applications (water impact, bird strike, explosive forming) where Lagrangian mesh tangling is problematic. The property data is stored in the SPH buffer initialised by `starter/source/elements/sph/`.

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `starter/source/elements/sph/README.md` — SPH element initialisation
- `engine/source/elements/sph/README.md` — SPH kernel computations
