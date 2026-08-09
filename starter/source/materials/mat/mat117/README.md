# Starter LAW117 — Elastic-plastic with Hosford-Coulomb fracture (`starter/source/materials/mat/mat117/`)

Reads `/MAT/LAW117` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat117.F` | `HM_READ_MAT117` — reads `/MAT/LAW117` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat117/README.md` — corresponding engine law
