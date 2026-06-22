# Starter LAW57 — Visco-elastic foam (`starter/source/materials/mat/mat057/`)

Reads `/MAT/LAW57` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `calculp2.F90` | `CALCULP2` — starter helper routine |
| `hm_read_mat57.F90` | `HM_READ_MAT57` — reads `/MAT/LAW57` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat057/README.md` — corresponding engine law
