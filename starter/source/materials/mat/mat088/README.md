# Starter LAW88 — Elastic-plastic with progressive delamination (`starter/source/materials/mat/mat088/`)

Reads `/MAT/LAW88` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat88.F90` | `HM_READ_MAT88` — reads `/MAT/LAW88` keyword parameters into PM/IPM |
| `sigeps88.F90` | `SIGEPS88` — starter helper routine |
| `table_mat_spline_fit_mod.F90` | `TABLE_MAT_SPLINE_FIT` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat088/README.md` — corresponding engine law
