# Matuser — User-Defined Material Reader (`starter/source/materials/mat/matuser/`)

Starter readers for user-defined material laws (LAW29, LAW31, LAW99) that
delegate computation to external FORTRAN/C routines at runtime.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat_user29_31.F` | `HM_READ_MAT29_31` — reads `/MAT/USER` parameters for user laws 29 and 31; stores user-supplied constants in `PM` |
| `hm_read_mat_user_99.F` | Reader for LAW99 user material: extended parameter set, supports ILAW flag selection |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
