# Starter LAW135 — Elastic-plastic with tabulated hardening and shear-banding Reader (`starter/source/materials/mat/mat135/`)

Reads `/MAT/LAW135` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat135.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat135/README.md` — corresponding engine law
