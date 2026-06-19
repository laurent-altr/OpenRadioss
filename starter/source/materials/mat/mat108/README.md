# Starter LAW108 — Elastic-plastic with forming-limit diagram (FLD) damage (`starter/source/materials/mat/mat108/`)

Reads `/MAT/LAW108` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat108.F` | `HM_READ_MAT108` — reads `/MAT/LAW108` keyword parameters into PM/IPM |
| `law108_upd.F` | `LAW108_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat108/README.md` — corresponding engine law
