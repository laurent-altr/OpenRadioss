# Starter LAW190 — Tabulated composite damage model (CDM) Reader (`starter/source/materials/mat/mat190/`)

Reads `/MAT/LAW190` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat190.F` | Starter input reader / initialiser |
| `law190_upd.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat190/README.md` — corresponding engine law
