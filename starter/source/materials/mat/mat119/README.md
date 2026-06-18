# Starter LAW119 — Elastic-plastic with tabulated hardening and BBC2005 anisotropy Reader (`starter/source/materials/mat/mat119/`)

Reads `/MAT/LAW119` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat119.F` | Starter input reader / initialiser |
| `law119_upd.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat119/README.md` — corresponding engine law
