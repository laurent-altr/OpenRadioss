# Starter LAW11 — Johnson-Holmquist ceramic (JH-1) Reader (`starter/source/materials/mat/mat011/`)

Reads `/MAT/LAW11` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat11.F` | Starter input reader / initialiser |
| `hm_read_mat11_k_eps.F` | Starter input reader / initialiser |
| `mat11check.F` | Starter input reader / initialiser |
| `nodm11.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat011/README.md` — corresponding engine law
