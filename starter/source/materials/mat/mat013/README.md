# Starter LAW13 — Elastic-plastic with combined isotropic/kinematic hardening Reader (`starter/source/materials/mat/mat013/`)

Reads `/MAT/LAW13` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat13.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat013/README.md` — corresponding engine law
