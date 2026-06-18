# Starter LAW58 — Visco-elastic with tabulated relaxation Reader (`starter/source/materials/mat/mat058/`)

Reads `/MAT/LAW58` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `cm58_refsta.F` | Starter input reader / initialiser |
| `cm58in3.F` | Starter input reader / initialiser |
| `hm_read_mat58.F` | Starter input reader / initialiser |
| `law58_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat058/README.md` — corresponding engine law
