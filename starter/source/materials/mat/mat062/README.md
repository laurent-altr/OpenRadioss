# Starter LAW62 — Elastic-plastic with Drucker-Prager + cap (geo-materials) Reader (`starter/source/materials/mat/mat062/`)

Reads `/MAT/LAW62` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat62.F` | Starter input reader / initialiser |
| `law62_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat062/README.md` — corresponding engine law
