# Starter LAW52 — Elastic-plastic with tabulated Barlat yield (`starter/source/materials/mat/mat052/`)

Reads `/MAT/LAW52` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat52.F` | `HM_READ_MAT52` — reads `/MAT/LAW52` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat052/README.md` — corresponding engine law
