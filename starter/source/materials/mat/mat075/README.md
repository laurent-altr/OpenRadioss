# Starter LAW75 — Elastic-plastic orthotropic (Hill 1948) with tabulated hardening Reader (`starter/source/materials/mat/mat075/`)

Reads `/MAT/LAW75` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat75.F` | Starter input reader / initialiser |
| `m75init.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat075/README.md` — corresponding engine law
