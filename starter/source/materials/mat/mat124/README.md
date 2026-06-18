# Starter LAW124 — Elastic-plastic with tabulated hardening and mixed Chaboche Reader (`starter/source/materials/mat/mat124/`)

Reads `/MAT/LAW124` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat124.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat124/README.md` — corresponding engine law
