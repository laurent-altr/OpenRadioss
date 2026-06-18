# Starter LAW132 — Elastic-plastic with tabulated hardening and modified GTN Reader (`starter/source/materials/mat/mat132/`)

Reads `/MAT/LAW132` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat132.F90` | Starter input reader / initialiser |
| `m132init.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat132/README.md` — corresponding engine law
