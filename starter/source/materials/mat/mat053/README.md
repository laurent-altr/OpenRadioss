# Starter LAW53 — Elastic-plastic with Yld2000-2d anisotropic yield (`starter/source/materials/mat/mat053/`)

Reads `/MAT/LAW53` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat53.F` | `HM_READ_MAT53` — reads `/MAT/LAW53` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat053/README.md` — corresponding engine law
