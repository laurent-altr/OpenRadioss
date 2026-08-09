# Starter LAW105 — Elastic-plastic with rate and thermal tabulation (LCSS) (`starter/source/materials/mat/mat105/`)

Reads `/MAT/LAW105` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat105.F90` | `HM_READ_MAT105` — reads `/MAT/LAW105` keyword parameters into PM/IPM |
| `m105init.F` | `M105INIT` — initialises PM array entries and state variables |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat105/README.md` — corresponding engine law
