# Starter LAW120 — Elastic-plastic with tabulated yield and damage (multi-surface) Reader (`starter/source/materials/mat/mat120/`)

Reads `/MAT/LAW120` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat120.F` | Starter input reader / initialiser |
| `law120_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat120/README.md` — corresponding engine law
