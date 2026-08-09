# LAW46 — Visco-Elastic (`engine/source/materials/mat/mat046/`)

Linear visco-elastic material: shear modulus represented by a Prony series
G(t) = G∞ + Σ Gₙ exp(−t/τₙ). Maxwell elements in parallel.

## Key Files

| File | Role |
|------|------|
| `m46law.F` | Main stress update: Prony series convolution integral |
| `sigeps46.F` | Generic solid stress-update wrapper |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/materials/visc/README.md` — shared Prony utilities
