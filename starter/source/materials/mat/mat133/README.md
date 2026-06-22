# Starter LAW133 — Elastic-plastic with tabulated hardening and ductile damage (`starter/source/materials/mat/mat133/`)

Reads `/MAT/LAW133` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat133.F90` | `HM_READ_MAT133` — reads `/MAT/LAW133` keyword parameters into PM/IPM |
| `law133_upd.F90` | `LAW133_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat133/README.md` — corresponding engine law
