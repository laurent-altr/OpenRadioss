# Starter LAW158 — Elastic-plastic with tabulated hardening (reserved / future) Reader (`starter/source/materials/mat/mat158/`)

Reads `/MAT/LAW158` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat158.F` | Starter input reader / initialiser |
| `law158_init.F` | Starter input reader / initialiser |
| `law158_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat158/README.md` — corresponding engine law
