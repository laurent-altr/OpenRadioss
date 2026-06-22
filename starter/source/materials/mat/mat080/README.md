# Starter LAW80 — Elastic-plastic rock (Drucker-Prager with tension cut-off) (`starter/source/materials/mat/mat080/`)

Reads `/MAT/LAW80` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat80.F` | `HM_READ_MAT80` — reads `/MAT/LAW80` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat080/README.md` — corresponding engine law
