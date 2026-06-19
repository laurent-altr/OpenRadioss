# Starter LAW15 — Elastic-plastic with progressive cracking (`starter/source/materials/mat/mat015/`)

Reads `/MAT/LAW15` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat15.F` | `HM_READ_MAT15` — reads `/MAT/LAW15` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat015/README.md` — corresponding engine law
