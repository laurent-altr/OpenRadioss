# Starter LAW132 — Elastic-plastic with tabulated hardening and modified GTN (`starter/source/materials/mat/mat132/`)

Reads `/MAT/LAW132` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat132.F90` | `HM_READ_MAT132` — reads `/MAT/LAW132` keyword parameters into PM/IPM |
| `m132init.F90` | `M132INIT` — initialises PM array entries and state variables |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat132/README.md` — corresponding engine law
