# LAW131 — Work Hardening Modules (`engine/source/materials/mat/mat131/work_hardening/`)

Modular isotropic hardening functions σ_y(εₚ) for the LAW131 framework.

## Key Files

| File | Role |
|------|------|
| `work_hardening_voce.F90` | Voce exponential saturation: σ = σ∞ − (σ∞−σ₀) exp(−cεₚ) |
| `work_hardening_linearvoce.F90` | Linear + Voce combined |
| `work_hardening_powerlaw.F90` | Power-law: σ = K εₚⁿ |
| `work_hardening_tabulated.F90` | Tabulated σ_y(εₚ) curve |

## Related Documentation

- `engine/source/materials/mat/mat131/README.md` — parent directory
