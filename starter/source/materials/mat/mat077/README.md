# Starter LAW77 — Elastic-plastic composite with Tsai-Hill failure (`starter/source/materials/mat/mat077/`)

Reads `/MAT/LAW77` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat77.F` | `HM_READ_MAT77` — reads `/MAT/LAW77` keyword parameters into PM/IPM |
| `law77_upd.F` | `LAW77_UPD` — post-read update: derived quantities, defaults, consistency checks |
| `m77init.F` | `M77INIT` — initialises PM array entries and state variables |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat077/README.md` — corresponding engine law
