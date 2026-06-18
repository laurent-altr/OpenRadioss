# Starter LAW81 — Elastic-plastic with piecewise linear hardening and failure Reader (`starter/source/materials/mat/mat081/`)

Reads `/MAT/LAW81` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat81.F90` | Starter input reader / initialiser |
| `law81_upd.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat081/README.md` — corresponding engine law
