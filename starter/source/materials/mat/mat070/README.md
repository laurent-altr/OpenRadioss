# Starter LAW70 — Elastic-plastic with Barlat Yld91 yield criterion (`starter/source/materials/mat/mat070/`)

Reads `/MAT/LAW70` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat70.F` | `HM_READ_MAT70` — reads `/MAT/LAW70` keyword parameters into PM/IPM |
| `law70_table.F` | `LAW70_TABLE` — starter helper routine |
| `law70_upd.F` | `LAW70_UPD` — post-read update: derived quantities, defaults, consistency checks |
| `m70init.F` | `M70INIT` — initialises PM array entries and state variables |
| `sigeps70.F` | `SIGEPS70` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat070/README.md` — corresponding engine law
