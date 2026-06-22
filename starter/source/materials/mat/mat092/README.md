# Starter LAW92 — Elastic-plastic Drucker-Prager with hardening (`starter/source/materials/mat/mat092/`)

Reads `/MAT/LAW92` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat92.F` | `HM_READ_MAT92` — reads `/MAT/LAW92` keyword parameters into PM/IPM |
| `law92_nlsqf.F90` | `LAW92_NLSQF` — starter helper routine |
| `law92_upd.F` | `LAW92_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat092/README.md` — corresponding engine law
