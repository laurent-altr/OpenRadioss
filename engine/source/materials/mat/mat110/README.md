# LAW110 — Composite Progressive Damage Shell (Ladeveze-Enhanced) (`engine/source/materials/mat/mat110/`)

Enhanced Ladeveze mesomodel for UD composite shells: fiber/matrix damage
plus plastic shear; Newton and NICE (vectorised) solvers; lite (reduced) variant.

## Key Files

| File | Role |
|------|------|
| `sigeps110c.F` | Shell dispatcher |
| `sigeps110c_newton.F` / `sigeps110c_nice.F` | Full Newton / NICE solvers |
| `sigeps110c_lite_newton.F` / `sigeps110c_lite_nice.F` | Lite solvers (fewer damage variables) |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
