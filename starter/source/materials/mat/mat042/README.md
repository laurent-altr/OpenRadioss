# Starter LAW42 — Arruda-Boyce hyperelastic Reader (`starter/source/materials/mat/mat042/`)

Reads `/MAT/LAW42` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat42.F` | Starter input reader / initialiser |
| `law42_upd.F` | Starter input reader / initialiser |
| `law42c_ini.F90` | Starter input reader / initialiser |
| `sigeps42.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat042/README.md` — corresponding engine law
