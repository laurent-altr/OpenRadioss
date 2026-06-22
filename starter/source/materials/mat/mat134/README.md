# Starter LAW134 — Elastic-plastic with tabulated hardening and Lemaitre damage (`starter/source/materials/mat/mat134/`)

Reads `/MAT/LAW134` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat134.F90` | `HM_READ_MAT134` — reads `/MAT/LAW134` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat134/README.md` — corresponding engine law
