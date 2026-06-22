# Starter LAW37 — Elastic foam with hysteresis (`starter/source/materials/mat/mat037/`)

Reads `/MAT/LAW37` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat37.F` | `HM_READ_MAT37` — reads `/MAT/LAW37` keyword parameters into PM/IPM |
| `m37init.F` | `M37INIT` — initialises PM array entries and state variables |
| `sigeps37.F` | `SIGEPS37` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat037/README.md` — corresponding engine law
