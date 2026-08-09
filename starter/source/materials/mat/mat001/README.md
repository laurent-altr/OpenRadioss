# Starter LAW1 — Elastic isotropic (Hooke's law) (`starter/source/materials/mat/mat001/`)

Reads `/MAT/LAW1` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat01.F` | `HM_READ_MAT01` — reads `/MAT/LAW1` keyword parameters into PM/IPM |
| `sigeps01.F90` | `SIGEPS01` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat001/README.md` — corresponding engine law
