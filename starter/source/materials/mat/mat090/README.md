# Starter LAW90 — Elastic-plastic with porous plasticity (Rousselier) Reader (`starter/source/materials/mat/mat090/`)

Reads `/MAT/LAW90` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat90.F` | Starter input reader / initialiser |
| `law90_upd.F` | Starter input reader / initialiser |
| `sigeps90.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat090/README.md` — corresponding engine law
