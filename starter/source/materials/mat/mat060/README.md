# Starter LAW60 — Elasto-visco-plastic (Chaboche) (`starter/source/materials/mat/mat060/`)

Reads `/MAT/LAW60` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat60.F` | `HM_READ_MAT60` — reads `/MAT/LAW60` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat060/README.md` — corresponding engine law
