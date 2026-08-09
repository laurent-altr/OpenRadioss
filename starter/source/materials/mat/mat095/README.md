# Starter LAW95 — Elastic-plastic with tabulated yield surface (`starter/source/materials/mat/mat095/`)

Reads `/MAT/LAW95` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat95.F` | `HM_READ_MAT95` — reads `/MAT/LAW95` keyword parameters into PM/IPM |
| `m95init.F` | `M95INIT` — initialises PM array entries and state variables |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat095/README.md` — corresponding engine law
