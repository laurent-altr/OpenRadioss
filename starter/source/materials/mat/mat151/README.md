# Starter LAW151 — Elastic-plastic with tabulated hardening (reserved / future) (`starter/source/materials/mat/mat151/`)

Reads `/MAT/LAW151` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat151.F` | `HM_READ_MAT151` — reads `/MAT/LAW151` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat151/README.md` — corresponding engine law
