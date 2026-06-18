# Starter LAW109 — Elastic-plastic with tabulated hardening and shear failure Reader (`starter/source/materials/mat/mat109/`)

Reads `/MAT/LAW109` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat109.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat109/README.md` — corresponding engine law
