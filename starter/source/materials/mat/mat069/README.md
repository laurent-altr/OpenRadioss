# Starter LAW69 — Elastic-plastic foam (low-density polyurethane) (`starter/source/materials/mat/mat069/`)

Reads `/MAT/LAW69` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat69.F` | `HM_READ_MAT69` — reads `/MAT/LAW69` keyword parameters into PM/IPM |
| `law69_upd.F` | `LAW69_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat069/README.md` — corresponding engine law
