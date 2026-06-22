# LAW27 — Elastic-Plastic with Prager-Ziegler Hardening (`engine/source/materials/mat/mat027/`)

Elastic-plastic material with nonlinear kinematic hardening (Prager-Ziegler
back-stress rule) for accurate Bauschinger-effect and cyclic plasticity.

## Key Files

| File | Role |
|------|------|
| `m27elas.F` | Elastic predictor |
| `m27plas.F` | Radial-return + Prager-Ziegler back-stress update |
| `m27crak.F` | Crack/failure flag management |
| `sigeps27c.F` | Shell stress-update wrapper |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
