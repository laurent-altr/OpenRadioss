# Starter LAW33 — Elastic-plastic with ductile damage (Lemaitre) (`starter/source/materials/mat/mat033/`)

Reads `/MAT/LAW33` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat33.F` | `HM_READ_MAT33` — reads `/MAT/LAW33` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat033/README.md` — corresponding engine law
