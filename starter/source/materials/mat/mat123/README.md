# Starter LAW123 — Elastic-plastic with tabulated hardening and shear fracture Reader (`starter/source/materials/mat/mat123/`)

Reads `/MAT/LAW123` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat123.F90` | Starter input reader / initialiser |
| `law123_upd.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat123/README.md` — corresponding engine law
