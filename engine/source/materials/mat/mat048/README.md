# LAW48 — Tabulated Elastic-Plastic with Failure (`engine/source/materials/mat/mat048/`)

Tabulated isotropic hardening with strain-rate dependence and element
deletion on a tabulated plastic-strain failure surface f(η, ε̇, T).

## Key Files

| File | Role |
|------|------|
| `sigeps48.F` | Main stress update: table interpolation + failure criterion |
| `sigeps48c.F` | Shell variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
