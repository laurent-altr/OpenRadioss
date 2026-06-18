# Starter LAW19 — Elastic-plastic shell with stress resultants Reader (`starter/source/materials/mat/mat019/`)

Reads `/MAT/LAW19` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat19.F` | Starter input reader / initialiser |
| `inepri.F` | Starter input reader / initialiser |
| `ininode_rm.F` | Starter input reader / initialiser |
| `law19_upd.F90` | Starter input reader / initialiser |
| `rigid_mat.F` | Starter input reader / initialiser |
| `valpr.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat019/README.md` — corresponding engine law
