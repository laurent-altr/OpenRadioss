# Starter LAW38 — Elastic-plastic (Hill 1948) with rate and thermal effects (`starter/source/materials/mat/mat038/`)

Reads `/MAT/LAW38` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat38.F` | `HM_READ_MAT38` — reads `/MAT/LAW38` keyword parameters into PM/IPM |
| `m38init.F` | `M38INIT` — initialises PM array entries and state variables |
| `sigeps38.F` | `SIGEPS38` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat038/README.md` — corresponding engine law
