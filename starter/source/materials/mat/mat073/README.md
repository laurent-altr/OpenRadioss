# Starter LAW73 — Elastic-plastic with tabulated hardening (BBC2005 yield) (`starter/source/materials/mat/mat073/`)

Reads `/MAT/LAW73` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat73.F` | `HM_READ_MAT73` — reads `/MAT/LAW73` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat073/README.md` — corresponding engine law
