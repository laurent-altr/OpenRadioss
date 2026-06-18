# Starter LAW77 — Elastic-plastic composite with Tsai-Hill failure Reader (`starter/source/materials/mat/mat077/`)

Reads `/MAT/LAW77` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat77.F` | Starter input reader / initialiser |
| `law77_upd.F` | Starter input reader / initialiser |
| `m77init.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat077/README.md` — corresponding engine law
