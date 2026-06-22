# Starter LAW4 — Elastic-plastic with kinematic hardening (`starter/source/materials/mat/mat004/`)

Reads `/MAT/LAW4` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat04.F` | `HM_READ_MAT04` — reads `/MAT/LAW4` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat004/README.md` — corresponding engine law
