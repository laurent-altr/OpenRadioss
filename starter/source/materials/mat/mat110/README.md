# Starter LAW110 — Elastic-plastic with tabulated hardening (Voce extended) (`starter/source/materials/mat/mat110/`)

Reads `/MAT/LAW110` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat110.F` | `HM_READ_MAT110` — reads `/MAT/LAW110` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat110/README.md` — corresponding engine law
