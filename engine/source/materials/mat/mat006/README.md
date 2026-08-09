# LAW6 — Viscoplastic Cowper-Symonds (`engine/source/materials/mat/mat006/`)

Elastic-plastic material with Cowper-Symonds strain-rate hardening.
σ_y(ε̇) = σ_y0 [1 + (ε̇/C)^{1/P}]. Common for mild steel and aluminium.

## Key Files

| File | Role |
|------|------|
| `m6law.F` | Main constitutive update: Cowper-Symonds rate correction |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
