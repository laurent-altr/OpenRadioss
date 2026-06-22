# Starter LAW42 — Arruda-Boyce hyperelastic (`starter/source/materials/mat/mat042/`)

Reads `/MAT/LAW42` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat42.F` | `HM_READ_MAT42` — reads `/MAT/LAW42` keyword parameters into PM/IPM |
| `law42_upd.F` | `LAW42_UPD` — post-read update: derived quantities, defaults, consistency checks |
| `law42c_ini.F90` | `LAW42C_INI` — starter helper routine |
| `sigeps42.F` | `SIGEPS42` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat042/README.md` — corresponding engine law
