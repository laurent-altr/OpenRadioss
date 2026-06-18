# Starter LAW68 — Elastic-plastic with mixed Chaboche hardening and damage Reader (`starter/source/materials/mat/mat068/`)

Reads `/MAT/LAW68` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat68.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat068/README.md` — corresponding engine law
