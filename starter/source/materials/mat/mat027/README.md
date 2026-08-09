# Starter LAW27 — Elastic-plastic with tabulated hardening and failure (`starter/source/materials/mat/mat027/`)

Reads `/MAT/LAW27` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `cm27in3.F` | `CM27IN3` — starter helper routine |
| `hm_read_mat27.F` | `HM_READ_MAT27` — reads `/MAT/LAW27` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat027/README.md` — corresponding engine law
