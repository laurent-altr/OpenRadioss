# Starter LAW1 — Elastic isotropic (Hooke's law) Reader (`starter/source/materials/mat/mat001/`)

Reads `/MAT/LAW1` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat01.F` | Starter input reader / initialiser |
| `sigeps01.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat001/README.md` — corresponding engine law
