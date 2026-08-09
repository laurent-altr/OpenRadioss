# Starter LAW10 — Elastic-plastic orthotropic — Barlat 1989 yield criterion (`starter/source/materials/mat/mat010/`)

Reads `/MAT/LAW10` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat10.F` | `HM_READ_MAT10` — reads `/MAT/LAW10` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat010/README.md` — corresponding engine law
