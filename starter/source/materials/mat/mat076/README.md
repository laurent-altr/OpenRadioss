# Starter LAW76 — Elastic-plastic with Barlat Yld2004-18p yield (`starter/source/materials/mat/mat076/`)

Reads `/MAT/LAW76` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat76.F` | `HM_READ_MAT76` — reads `/MAT/LAW76` keyword parameters into PM/IPM |
| `law76_func_comp.F90` | `LAW76_FUNC_COMP` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat076/README.md` — corresponding engine law
