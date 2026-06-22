# Starter LAW46 — Elastic-plastic with Drucker-Prager yield (geo-materials) (`starter/source/materials/mat/mat046/`)

Reads `/MAT/LAW46` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat46.F` | `HM_READ_MAT46` — reads `/MAT/LAW46` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat046/README.md` — corresponding engine law
