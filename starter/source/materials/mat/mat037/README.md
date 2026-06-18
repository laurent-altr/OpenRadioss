# Starter LAW37 — Elastic foam with hysteresis Reader (`starter/source/materials/mat/mat037/`)

Reads `/MAT/LAW37` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat37.F` | Starter input reader / initialiser |
| `m37init.F` | Starter input reader / initialiser |
| `sigeps37.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat037/README.md` — corresponding engine law
