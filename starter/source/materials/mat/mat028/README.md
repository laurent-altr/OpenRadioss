# Starter LAW28 — Elastic-plastic with tabulated hardening (Vegter yield) (`starter/source/materials/mat/mat028/`)

Reads `/MAT/LAW28` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat28.F` | `HM_READ_MAT28` — reads `/MAT/LAW28` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat028/README.md` — corresponding engine law
