# Starter LAW111 — Elastic-plastic with tabulated strain-rate and temperature (`starter/source/materials/mat/mat111/`)

Reads `/MAT/LAW111` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat111.F` | `HM_READ_MAT111` — reads `/MAT/LAW111` keyword parameters into PM/IPM |
| `law111_upd.F` | `LAW111_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat111/README.md` — corresponding engine law
