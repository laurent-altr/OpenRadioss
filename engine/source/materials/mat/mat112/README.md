# LAW112 — Composite Damage (Xia Model) (`engine/source/materials/mat/mat112/`)

Orthotropic composite damage model with the Xia approach: Weibull-based
fiber strength distribution + interlaminar shear damage. Newton/NICE solvers.

## Key Files

| File | Role |
|------|------|
| `sigeps112.F` / `sigeps112c.F` | Main solid / shell dispatcher |
| `mat112_xia_newton.F` / `mat112_xia_nice.F` | Solid solvers |
| `mat112c_xia_newton.F` / `mat112c_xia_nice.F` | Shell solvers |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
