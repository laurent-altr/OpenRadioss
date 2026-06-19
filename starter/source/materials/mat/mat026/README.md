# Starter LAW26 — Honeycomb / open-cell foam (tabulated) (`starter/source/materials/mat/mat026/`)

Reads `/MAT/LAW26` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat26.F` | `HM_READ_MAT26` — reads `/MAT/LAW26` keyword parameters into PM/IPM |
| `mindex.F` | Starter helper |
| `mrdse2.F` | `MRDSE2` — starter helper routine |
| `mrdse3.F` | `MRDSE3` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat026/README.md` — corresponding engine law
