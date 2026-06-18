# Starter LAW70 — Elastic-plastic with Barlat Yld91 yield criterion Reader (`starter/source/materials/mat/mat070/`)

Reads `/MAT/LAW70` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat70.F` | Starter input reader / initialiser |
| `law70_table.F` | Starter input reader / initialiser |
| `law70_upd.F` | Starter input reader / initialiser |
| `m70init.F` | Starter input reader / initialiser |
| `sigeps70.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat070/README.md` — corresponding engine law
