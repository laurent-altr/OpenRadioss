# LAW115 — Thermoplastic (Polymer) (`engine/source/materials/mat/mat115/`)

Elastic-visco-plastic model for thermoplastics: temperature- and
rate-dependent yield with strain-induced softening and recovery.

## Key Files

| File | Role |
|------|------|
| `sigeps115.F` | Main stress update: thermoplastic visco-plasticity |
| `mat115_newton.F` / `mat115_nice.F` | Newton / NICE (vectorised) solvers |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
