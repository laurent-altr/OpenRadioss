# Starter LAW64 — Elastic-plastic with tabulated isotropic hardening and damage (`starter/source/materials/mat/mat064/`)

Reads `/MAT/LAW64` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat64.F` | `HM_READ_MAT64` — reads `/MAT/LAW64` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat064/README.md` — corresponding engine law
