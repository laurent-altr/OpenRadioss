# Starter LAW128 — Elastic-plastic with tabulated hardening and BBC2008 damage (`starter/source/materials/mat/mat128/`)

Reads `/MAT/LAW128` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat128.F90` | `HM_READ_MAT128` — reads `/MAT/LAW128` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat128/README.md` — corresponding engine law
