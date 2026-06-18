# Starter LAW38 — Elastic-plastic (Hill 1948) with rate and thermal effects Reader (`starter/source/materials/mat/mat038/`)

Reads `/MAT/LAW38` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat38.F` | Starter input reader / initialiser |
| `m38init.F` | Starter input reader / initialiser |
| `sigeps38.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat038/README.md` — corresponding engine law
