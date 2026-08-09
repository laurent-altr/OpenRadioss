# Starter LAW25 — Composite with Tsai-Wu failure and delamination (`starter/source/materials/mat/mat025/`)

Reads `/MAT/LAW25` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat25.F` | `HM_READ_MAT25` — reads `/MAT/LAW25` keyword parameters into PM/IPM |
| `read_mat25_crasurv.F90` | `READ_MAT25_CRASURV` — starter helper routine |
| `read_mat25_tsaiwu.F90` | `READ_MAT25_TSAIWU` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat025/README.md` — corresponding engine law
