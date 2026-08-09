# Starter LAW119 — Elastic-plastic with tabulated hardening and BBC2005 anisotropy (`starter/source/materials/mat/mat119/`)

Reads `/MAT/LAW119` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat119.F` | `HM_READ_MAT119` — reads `/MAT/LAW119` keyword parameters into PM/IPM |
| `law119_upd.F` | `LAW119_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat119/README.md` — corresponding engine law
