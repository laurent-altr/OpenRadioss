# Starter LAW127 — Elastic-plastic with tabulated hardening and forming limit (`starter/source/materials/mat/mat127/`)

Reads `/MAT/LAW127` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat127.F90` | `HM_READ_MAT127` — reads `/MAT/LAW127` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat127/README.md` — corresponding engine law
