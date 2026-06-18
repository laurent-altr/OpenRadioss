# Starter LAW111 — Elastic-plastic with tabulated strain-rate and temperature Reader (`starter/source/materials/mat/mat111/`)

Reads `/MAT/LAW111` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat111.F` | Starter input reader / initialiser |
| `law111_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat111/README.md` — corresponding engine law
