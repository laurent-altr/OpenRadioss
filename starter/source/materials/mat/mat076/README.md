# Starter LAW76 — Elastic-plastic with Barlat Yld2004-18p yield Reader (`starter/source/materials/mat/mat076/`)

Reads `/MAT/LAW76` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat76.F` | Starter input reader / initialiser |
| `law76_func_comp.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat076/README.md` — corresponding engine law
