# LAW2 — Johnson-Cook Elastic-Plastic (`engine/source/materials/mat/mat002/`)

Elastic-plastic material with Johnson-Cook isotropic hardening and optional
strain-rate sensitivity. Widely used for metals under dynamic loading.
σ_y = (A + B εₚⁿ)(1 + C ln ε̇*)(1 − T*ᵐ).

## Key Files

| File | Role |
|------|------|
| `m2law.F` | Main constitutive update: radial return, JC hardening |
| `m2law8.F` | 8-node solid variant |
| `m2lawp.F` | Plane-stress projection |
| `m2lawpi.F` | Plane-stress incompressible |
| `m2lawt.F` | Thermal coupling |
| `m2cplr.F` | Material coupler for composite layers |
| `m2iter_imp.F` | Implicit iteration for return mapping |
| `sigeps02c.F` / `sigeps02g.F` | Shell / SPH wrappers |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
