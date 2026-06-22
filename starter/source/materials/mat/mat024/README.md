# Starter LAW24 — Composite progressive damage (Ladeveze/Allix) (`starter/source/materials/mat/mat024/`)

Reads `/MAT/LAW24` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat24.F` | `HM_READ_MAT24` — reads `/MAT/LAW24` keyword parameters into PM/IPM |
| `m24in2.F` | `M24IN2` — starter helper routine |
| `m24in3.F` | `M24IN3` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat024/README.md` — corresponding engine law
