# Starter LAW0 — Rigid body / void material (placeholder) (`starter/source/materials/mat/mat000/`)

Reads `/MAT/LAW0` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat00.F` | `HM_READ_MAT00` — reads `/MAT/LAW0` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat000/README.md` — corresponding engine law
