# Starter LAW66 — Elastic-plastic with GTN damage and shear correction (`starter/source/materials/mat/mat066/`)

Reads `/MAT/LAW66` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat66.F` | `HM_READ_MAT66` — reads `/MAT/LAW66` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat066/README.md` — corresponding engine law
