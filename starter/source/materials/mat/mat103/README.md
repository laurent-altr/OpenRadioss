# Starter LAW103 — Elastic-plastic with tabulated hardening and BBC2008 yield (`starter/source/materials/mat/mat103/`)

Reads `/MAT/LAW103` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat103.F` | `HM_READ_MAT103` — reads `/MAT/LAW103` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat103/README.md` — corresponding engine law
