# Starter LAW48 — Porous elastic-plastic (soil / crushable foam) (`starter/source/materials/mat/mat048/`)

Reads `/MAT/LAW48` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat48.F` | `HM_READ_MAT48` — reads `/MAT/LAW48` keyword parameters into PM/IPM |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat048/README.md` — corresponding engine law
