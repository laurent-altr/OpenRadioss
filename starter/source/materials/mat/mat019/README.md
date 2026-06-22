# Starter LAW19 — Elastic-plastic shell with stress resultants (`starter/source/materials/mat/mat019/`)

Reads `/MAT/LAW19` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat19.F` | `HM_READ_MAT19` — reads `/MAT/LAW19` keyword parameters into PM/IPM |
| `inepri.F` | `INEPRI` — starter helper routine |
| `ininode_rm.F` | `ININODE_RM` — starter helper routine |
| `law19_upd.F90` | `LAW19_UPD` — post-read update: derived quantities, defaults, consistency checks |
| `rigid_mat.F` | `RIGID_MAT` — starter helper routine |
| `valpr.F` | `VALPR` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat019/README.md` — corresponding engine law
