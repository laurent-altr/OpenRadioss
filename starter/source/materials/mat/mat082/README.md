# Starter LAW82 — Elastic-plastic with tabulated orthotropic hardening (`starter/source/materials/mat/mat082/`)

Reads `/MAT/LAW82` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat82.F` | `HM_READ_MAT82` — reads `/MAT/LAW82` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat082/README.md` — corresponding engine law
