# Starter LAW36 — Elastic-plastic with Gurson-Tvergaard-Needleman (GTN) void growth Reader (`starter/source/materials/mat/mat036/`)

Reads `/MAT/LAW36` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat36.F` | Starter input reader / initialiser |
| `law36_upd.F` | Starter input reader / initialiser |
| `m36init.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat036/README.md` — corresponding engine law
