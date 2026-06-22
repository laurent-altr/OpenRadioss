# Starter LAW81 — Elastic-plastic with piecewise linear hardening and failure (`starter/source/materials/mat/mat081/`)

Reads `/MAT/LAW81` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat81.F90` | `HM_READ_MAT81` — reads `/MAT/LAW81` keyword parameters into PM/IPM |
| `law81_upd.F90` | `LAW81_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat081/README.md` — corresponding engine law
