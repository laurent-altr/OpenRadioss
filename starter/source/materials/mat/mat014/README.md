# Starter LAW14 — Elastic-plastic with anisotropic damage (Ladeveze) (`starter/source/materials/mat/mat014/`)

Reads `/MAT/LAW14` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat14.F` | `HM_READ_MAT14` — reads `/MAT/LAW14` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat014/README.md` — corresponding engine law
