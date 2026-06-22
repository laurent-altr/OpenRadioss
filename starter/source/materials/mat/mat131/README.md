# Starter LAW131 — Modular elastic-plastic (pluggable elasticity/yield/hardening/return-mapping) (`starter/source/materials/mat/mat131/`)

Reads `/MAT/LAW131` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_elasto_plastic.F90` | `HM_READ_ELASTO_PLASTIC` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat131/README.md` — corresponding engine law
