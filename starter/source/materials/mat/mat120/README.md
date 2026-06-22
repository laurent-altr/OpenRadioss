# Starter LAW120 — Elastic-plastic with tabulated yield and damage (multi-surface) (`starter/source/materials/mat/mat120/`)

Reads `/MAT/LAW120` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat120.F` | `HM_READ_MAT120` — reads `/MAT/LAW120` keyword parameters into PM/IPM |
| `law120_upd.F` | `LAW120_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat120/README.md` — corresponding engine law
