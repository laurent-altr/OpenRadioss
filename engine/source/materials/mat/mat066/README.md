# LAW66 — Linear Visco-Elastic (Prony / Boltzmann) (`engine/source/materials/mat/mat066/`)

Linear visco-elastic material with Prony-series relaxation:
G(t) = G∞ + Σ Gₙ exp(−t/τₙ). Implemented via incremental Boltzmann superposition.

## Key Files

| File | Role |
|------|------|
| `sigeps66.F` | Main stress update: Prony convolution with recurrence |
| `sigeps66c.F` | Shell variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/materials/visc/README.md` — shared Prony utilities
