# Starter LAW78 — Elastic-plastic with rate-dependent Cowper-Symonds and damage (`starter/source/materials/mat/mat078/`)

Reads `/MAT/LAW78` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat78.F` | `HM_READ_MAT78` — reads `/MAT/LAW78` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat078/README.md` — corresponding engine law
