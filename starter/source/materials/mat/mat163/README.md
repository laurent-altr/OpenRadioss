# Starter LAW163 — Elastic-plastic with tabulated hardening (reserved / future) Reader (`starter/source/materials/mat/mat163/`)

Reads `/MAT/LAW163` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat163.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat163/README.md` — corresponding engine law
