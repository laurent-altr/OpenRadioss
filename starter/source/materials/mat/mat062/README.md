# Starter LAW62 — Elastic-plastic with Drucker-Prager + cap (geo-materials) (`starter/source/materials/mat/mat062/`)

Reads `/MAT/LAW62` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat62.F` | `HM_READ_MAT62` — reads `/MAT/LAW62` keyword parameters into PM/IPM |
| `law62_upd.F` | `LAW62_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat062/README.md` — corresponding engine law
