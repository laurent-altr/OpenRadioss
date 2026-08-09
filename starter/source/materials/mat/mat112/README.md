# Starter LAW112 — Elastic-plastic with Modified Mohr-Coulomb fracture criterion (`starter/source/materials/mat/mat112/`)

Reads `/MAT/LAW112` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat112.F` | `HM_READ_MAT112` — reads `/MAT/LAW112` keyword parameters into PM/IPM |
| `law112_upd.F` | `LAW112_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat112/README.md` — corresponding engine law
