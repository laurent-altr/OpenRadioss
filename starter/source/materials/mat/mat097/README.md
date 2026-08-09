# Starter LAW97 — Elastic-plastic with modified Bai-Wierzbicki fracture locus (`starter/source/materials/mat/mat097/`)

Reads `/MAT/LAW97` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat97.F` | `HM_READ_MAT97` — reads `/MAT/LAW97` keyword parameters into PM/IPM |
| `m97init.F` | `M97INIT` — initialises PM array entries and state variables |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat097/README.md` — corresponding engine law
