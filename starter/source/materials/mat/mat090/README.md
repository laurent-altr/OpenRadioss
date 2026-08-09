# Starter LAW90 — Elastic-plastic with porous plasticity (Rousselier) (`starter/source/materials/mat/mat090/`)

Reads `/MAT/LAW90` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat90.F` | `HM_READ_MAT90` — reads `/MAT/LAW90` keyword parameters into PM/IPM |
| `law90_upd.F` | `LAW90_UPD` — post-read update: derived quantities, defaults, consistency checks |
| `sigeps90.F` | `SIGEPS90` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat090/README.md` — corresponding engine law
