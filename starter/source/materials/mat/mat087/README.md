# Starter LAW87 — Visco-elastic Kelvin-Voigt foam Reader (`starter/source/materials/mat/mat087/`)

Reads `/MAT/LAW87` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat87.F90` | Starter input reader / initialiser |
| `law87_upd.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat087/README.md` — corresponding engine law
