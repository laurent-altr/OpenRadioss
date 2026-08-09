# Starter LAW125 — Elastic-plastic with tabulated anisotropy (Vegter 2017) (`starter/source/materials/mat/mat125/`)

Reads `/MAT/LAW125` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat125.F90` | `HM_READ_MAT125` — reads `/MAT/LAW125` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat125/README.md` — corresponding engine law
