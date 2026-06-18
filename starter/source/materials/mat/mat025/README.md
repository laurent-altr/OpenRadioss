# Starter LAW25 — Composite with Tsai-Wu failure and delamination Reader (`starter/source/materials/mat/mat025/`)

Reads `/MAT/LAW25` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat25.F` | Starter input reader / initialiser |
| `read_mat25_crasurv.F90` | Starter input reader / initialiser |
| `read_mat25_tsaiwu.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat025/README.md` — corresponding engine law
