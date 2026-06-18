# Starter LAW104 — Elastic-plastic with NICE / CPPM return-mapping and damage Reader (`starter/source/materials/mat/mat104/`)

Reads `/MAT/LAW104` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `cnloc_mat104_ini.F` | Starter input reader / initialiser |
| `hm_read_mat104.F` | Starter input reader / initialiser |
| `law104_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat104/README.md` — corresponding engine law
