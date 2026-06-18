# Starter LAW3 — Elastic-plastic with isotropic hardening (piecewise linear) Reader (`starter/source/materials/mat/mat003/`)

Reads `/MAT/LAW3` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat03.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat003/README.md` — corresponding engine law
