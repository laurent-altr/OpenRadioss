# Starter LAW2 — Johnson-Cook elastic-plastic with strain-rate and thermal softening Reader (`starter/source/materials/mat/mat002/`)

Reads `/MAT/LAW2` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat02_jc.F90` | Starter input reader / initialiser |
| `hm_read_mat02_predef.F90` | Starter input reader / initialiser |
| `hm_read_mat02_zerilli.F90` | Starter input reader / initialiser |
| `law02_upd.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat002/README.md` — corresponding engine law
