# Starter LAW115 — Elastic-plastic with combined Yoshida-Uemori and damage Reader (`starter/source/materials/mat/mat115/`)

Reads `/MAT/LAW115` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat115.F` | Starter input reader / initialiser |
| `m115_perturb.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat115/README.md` — corresponding engine law
