# Starter LAW190 — Tabulated composite damage model (CDM) (`starter/source/materials/mat/mat190/`)

Reads `/MAT/LAW190` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat190.F` | `HM_READ_MAT190` — reads `/MAT/LAW190` keyword parameters into PM/IPM |
| `law190_upd.F90` | `LAW190_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat190/README.md` — corresponding engine law
