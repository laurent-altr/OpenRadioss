# Starter LAW121 — Elastic-plastic with tabulated hardening and Swift-Voce (`starter/source/materials/mat/mat121/`)

Reads `/MAT/LAW121` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat121.F` | `HM_READ_MAT121` — reads `/MAT/LAW121` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat121/README.md` — corresponding engine law
