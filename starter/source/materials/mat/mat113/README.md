# Starter LAW113 — Elastic-plastic with Yoshida-Uemori kinematic hardening Reader (`starter/source/materials/mat/mat113/`)

Reads `/MAT/LAW113` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat113.F` | Starter input reader / initialiser |
| `law113_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat113/README.md` — corresponding engine law
