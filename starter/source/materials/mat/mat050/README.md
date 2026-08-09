# Starter LAW50 — Explosive burn with Jones-Wilkins-Lee (JWL) EOS (`starter/source/materials/mat/mat050/`)

Reads `/MAT/LAW50` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat50.F90` | `HM_READ_MAT50` — reads `/MAT/LAW50` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat050/README.md` — corresponding engine law
