# Starter LAW72 — Elastic-plastic with Prony visco-elasticity and damage (`starter/source/materials/mat/mat072/`)

Reads `/MAT/LAW72` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat72.F` | `HM_READ_MAT72` — reads `/MAT/LAW72` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat072/README.md` — corresponding engine law
