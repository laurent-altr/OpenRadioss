# Starter LAW113 — Elastic-plastic with Yoshida-Uemori kinematic hardening (`starter/source/materials/mat/mat113/`)

Reads `/MAT/LAW113` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat113.F` | `HM_READ_MAT113` — reads `/MAT/LAW113` keyword parameters into PM/IPM |
| `law113_upd.F` | `LAW113_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat113/README.md` — corresponding engine law
