# Starter LAW27 — Elastic-plastic with tabulated hardening and failure Reader (`starter/source/materials/mat/mat027/`)

Reads `/MAT/LAW27` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `cm27in3.F` | Starter input reader / initialiser |
| `hm_read_mat27.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat027/README.md` — corresponding engine law
