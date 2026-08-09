# Starter LAW11 — Johnson-Holmquist ceramic (JH-1) (`starter/source/materials/mat/mat011/`)

Reads `/MAT/LAW11` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat11.F` | `HM_READ_MAT11` — reads `/MAT/LAW11` keyword parameters into PM/IPM |
| `hm_read_mat11_k_eps.F` | `HM_READ_MAT11_K_EPS` — reads `/MAT/LAW11` k eps variant |
| `mat11check.F` | `MAT11CHECK` — input validation / sanity checks |
| `nodm11.F` | `NODM11` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat011/README.md` — corresponding engine law
