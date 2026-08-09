# Starter LAW83 — Composite shell with intralaminar and interlaminar damage (`starter/source/materials/mat/mat083/`)

Reads `/MAT/LAW83` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat83.F` | `HM_READ_MAT83` — reads `/MAT/LAW83` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat083/README.md` — corresponding engine law
