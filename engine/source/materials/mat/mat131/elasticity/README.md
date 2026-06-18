# LAW131 — Elasticity Modules (`engine/source/materials/mat/mat131/elasticity/`)

Modular elasticity implementations for the LAW131 framework.
Each file provides a self-contained Fortran 90 module.

## Key Files

| File | Role |
|------|------|
| `elasticity_isotropic.F90` | Isotropic Hooke's law (E, ν) |
| `elasticity_anisotropic.F90` | Full anisotropic stiffness tensor |
| `elasticity_orthotropic.F90` | Orthotropic (9 constants) |
| `elasticity_temp_isotropic.F90` | Temperature-dependent isotropic |
| `elasticity_bimod_isotropic.F90` | Bimodular (tension/compression) isotropic |
| `elasticity_viscous_isotropic.F90` | Visco-elastic isotropic correction |

## Related Documentation

- `engine/source/materials/mat/mat131/README.md` — parent directory
