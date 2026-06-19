# Starter LAW74 — Elastic-plastic with tabulated hardening and anisotropic damage (`starter/source/materials/mat/mat074/`)

Reads `/MAT/LAW74` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat74.F` | `HM_READ_MAT74` — reads `/MAT/LAW74` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat074/README.md` — corresponding engine law
