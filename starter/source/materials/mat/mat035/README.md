# Starter LAW35 — Elastic-plastic foam with volumetric locking Reader (`starter/source/materials/mat/mat035/`)

Reads `/MAT/LAW35` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `cm35in3.F` | Starter input reader / initialiser |
| `hm_read_mat35.F` | Starter input reader / initialiser |
| `sigeps35.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat035/README.md` — corresponding engine law
