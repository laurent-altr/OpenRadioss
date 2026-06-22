# Starter LAW75 — Elastic-plastic orthotropic (Hill 1948) with tabulated hardening (`starter/source/materials/mat/mat075/`)

Reads `/MAT/LAW75` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat75.F` | `HM_READ_MAT75` — reads `/MAT/LAW75` keyword parameters into PM/IPM |
| `m75init.F` | `M75INIT` — initialises PM array entries and state variables |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat075/README.md` — corresponding engine law
