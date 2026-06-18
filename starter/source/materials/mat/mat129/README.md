# Starter LAW129 — Elastic-plastic with tabulated hardening and Yld2000 damage Reader (`starter/source/materials/mat/mat129/`)

Reads `/MAT/LAW129` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat129.F90` | Starter input reader / initialiser |
| `law129_init.F90` | Starter input reader / initialiser |
| `law129_upd.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat129/README.md` — corresponding engine law
