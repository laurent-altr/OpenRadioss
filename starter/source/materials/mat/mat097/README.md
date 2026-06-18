# Starter LAW97 — Elastic-plastic with modified Bai-Wierzbicki fracture locus Reader (`starter/source/materials/mat/mat097/`)

Reads `/MAT/LAW97` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat97.F` | Starter input reader / initialiser |
| `m97init.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat097/README.md` — corresponding engine law
