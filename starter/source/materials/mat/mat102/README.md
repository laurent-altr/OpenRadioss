# Starter LAW102 — Elastic-plastic composite with Cuntze failure mode concept (`starter/source/materials/mat/mat102/`)

Reads `/MAT/LAW102` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat102.F` | `HM_READ_MAT102` — reads `/MAT/LAW102` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat102/README.md` — corresponding engine law
