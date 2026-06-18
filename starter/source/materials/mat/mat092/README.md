# Starter LAW92 — Elastic-plastic Drucker-Prager with hardening Reader (`starter/source/materials/mat/mat092/`)

Reads `/MAT/LAW92` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat92.F` | Starter input reader / initialiser |
| `law92_nlsqf.F90` | Starter input reader / initialiser |
| `law92_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat092/README.md` — corresponding engine law
