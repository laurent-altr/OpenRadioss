# Starter LAW158 — Elastic-plastic with tabulated hardening (reserved / future) (`starter/source/materials/mat/mat158/`)

Reads `/MAT/LAW158` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat158.F` | `HM_READ_MAT158` — reads `/MAT/LAW158` keyword parameters into PM/IPM |
| `law158_init.F` | `LAW158_INIT` — initialises PM array entries and state variables |
| `law158_upd.F` | `LAW158_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat158/README.md` — corresponding engine law
