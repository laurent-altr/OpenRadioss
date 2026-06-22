# Starter LAW58 — Visco-elastic with tabulated relaxation (`starter/source/materials/mat/mat058/`)

Reads `/MAT/LAW58` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `cm58_refsta.F` | `CM58_REFSTA` — starter helper routine |
| `cm58in3.F` | `CM58IN3` — starter helper routine |
| `hm_read_mat58.F` | `HM_READ_MAT58` — reads `/MAT/LAW58` keyword parameters into PM/IPM |
| `law58_upd.F` | `LAW58_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat058/README.md` — corresponding engine law
