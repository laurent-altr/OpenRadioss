# Starter LAW114 — Elastic-plastic with tabulated hardening and BBC2000 yield (`starter/source/materials/mat/mat114/`)

Reads `/MAT/LAW114` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat114.F` | `HM_READ_MAT114` — reads `/MAT/LAW114` keyword parameters into PM/IPM |
| `law114_upd.F` | `LAW114_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat114/README.md` — corresponding engine law
