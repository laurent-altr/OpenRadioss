# Starter LAW36 — Elastic-plastic with Gurson-Tvergaard-Needleman (GTN) void growth (`starter/source/materials/mat/mat036/`)

Reads `/MAT/LAW36` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat36.F` | `HM_READ_MAT36` — reads `/MAT/LAW36` keyword parameters into PM/IPM |
| `law36_upd.F` | `LAW36_UPD` — post-read update: derived quantities, defaults, consistency checks |
| `m36init.F90` | `M36INIT` — initialises PM array entries and state variables |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat036/README.md` — corresponding engine law
