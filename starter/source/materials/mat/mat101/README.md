# Starter LAW101 — Elastic-plastic with tabulated Barlat Yld2000-2d Reader (`starter/source/materials/mat/mat101/`)

Reads `/MAT/LAW101` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat101.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat101/README.md` — corresponding engine law
