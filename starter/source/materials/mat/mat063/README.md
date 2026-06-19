# Starter LAW63 — Elastic-plastic with tabulated anisotropy (Banabic BBC2000) (`starter/source/materials/mat/mat063/`)

Reads `/MAT/LAW63` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat63.F` | `HM_READ_MAT63` — reads `/MAT/LAW63` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat063/README.md` — corresponding engine law
