# Starter LAW112 — Elastic-plastic with Modified Mohr-Coulomb fracture criterion Reader (`starter/source/materials/mat/mat112/`)

Reads `/MAT/LAW112` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat112.F` | Starter input reader / initialiser |
| `law112_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat112/README.md` — corresponding engine law
