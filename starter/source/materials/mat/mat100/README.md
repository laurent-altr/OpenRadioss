# Starter LAW100 — Hyperelastic with Prony visco-elasticity (Arruda-Boyce / poly / power / sinh) (`starter/source/materials/mat/mat100/`)

Reads `/MAT/LAW100` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat100.F` | `HM_READ_MAT100` — reads `/MAT/LAW100` keyword parameters into PM/IPM |
| `law100_upd.F` | `LAW100_UPD_NHT` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat100/README.md` — corresponding engine law
