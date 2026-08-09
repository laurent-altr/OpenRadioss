# Starter LAW12 — Elastic-plastic with tabulated hardening (`starter/source/materials/mat/mat012/`)

Reads `/MAT/LAW12` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat12.F` | `HM_READ_MAT12` — reads `/MAT/LAW12` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat012/README.md` — corresponding engine law
