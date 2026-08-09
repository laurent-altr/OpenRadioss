# Starter LAW35 — Elastic-plastic foam with volumetric locking (`starter/source/materials/mat/mat035/`)

Reads `/MAT/LAW35` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `cm35in3.F` | `CM35IN3` — starter helper routine |
| `hm_read_mat35.F` | `HM_READ_MAT35` — reads `/MAT/LAW35` keyword parameters into PM/IPM |
| `sigeps35.F` | `SIGEPS35` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat035/README.md` — corresponding engine law
