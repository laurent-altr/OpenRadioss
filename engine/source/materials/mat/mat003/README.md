# LAW3 — Elastic-Plastic with Isotropic Hardening (`engine/source/materials/mat/mat003/`)

Elastic-plastic material using a piecewise-linear (tabulated) isotropic
hardening curve σ_y(εₚ), with optional strain-rate scaling.

## Key Files

| File | Role |
|------|------|
| `m3law.F` | Main constitutive update: radial return with table interpolation |
| `m3law8.F` | 8-node solid variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
