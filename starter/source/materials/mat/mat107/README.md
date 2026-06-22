# Starter LAW107 — Elastic-plastic with tabulated hardening and Yld2004 anisotropy (`starter/source/materials/mat/mat107/`)

Reads `/MAT/LAW107` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat107.F` | `HM_READ_MAT107` — reads `/MAT/LAW107` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat107/README.md` — corresponding engine law
