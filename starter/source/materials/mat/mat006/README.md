# Starter LAW6 — Viscoplastic Cowper-Symonds (`starter/source/materials/mat/mat006/`)

Reads `/MAT/LAW6` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat06.F` | `HM_READ_MAT06` — reads `/MAT/LAW6` keyword parameters into PM/IPM |
| `hm_read_mat06_keps.F` | `HM_READ_MAT06_KEPS` — reads `/MAT/LAW6` keps variant |
| `paroi.F` | `PAROI` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat006/README.md` — corresponding engine law
