# Starter LAW130 — Elastic-plastic with tabulated hardening and GTN damage (`starter/source/materials/mat/mat130/`)

Reads `/MAT/LAW130` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat130.F90` | `HM_READ_MAT130` — reads `/MAT/LAW130` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat130/README.md` — corresponding engine law
