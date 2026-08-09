# LAW18 — Elastic-Plastic with Thermal Coupling (`engine/source/materials/mat/mat018/`)

Elastic-plastic material with temperature-dependent yield stress and
thermal softening; supports Fourier heat conduction coupling.

## Key Files

| File | Role |
|------|------|
| `m18law.F` | Main constitutive update: plasticity with T-dependent σ_y |
| `m18th.F` | Thermal energy balance and heat generation from plastic work |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
