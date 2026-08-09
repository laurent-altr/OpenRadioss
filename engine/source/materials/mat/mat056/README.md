# LAW56 — Honeycomb (Tabulated Anisotropic) (`engine/source/materials/mat/mat056/`)

Fully anisotropic honeycomb: independent tabulated curves for each of six
normal/shear components; accounts for strain-rate effects.

## Key Files

| File | Role |
|------|------|
| `sigeps56.F` | Main stress update: 6-component lookup |
| `sigeps56c.F` | Shell variant |
| `sigeps56g.F` | SPH variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
