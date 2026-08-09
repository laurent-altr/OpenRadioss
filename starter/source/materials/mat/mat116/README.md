# Starter LAW116 — Elastic-plastic with tabulated hardening and Yld2000 anisotropy (`starter/source/materials/mat/mat116/`)

Reads `/MAT/LAW116` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat116.F` | `HM_READ_MAT116` — reads `/MAT/LAW116` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat116/README.md` — corresponding engine law
