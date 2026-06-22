# LAW107 — Progressive Composite Damage Newton/NICE (`engine/source/materials/mat/mat107/`)

Composite progressive failure model with inter-fibre fracture plane criterion;
two numerical solvers: Newton (robust) and NICE (vectorised).

## Key Files

| File | Role |
|------|------|
| `sigeps107.F` / `sigeps107c.F` | Main solid / shell dispatcher |
| `mat107_newton.F` / `mat107_nice.F` | Solid solvers |
| `mat107c_newton.F` / `mat107c_nice.F` | Shell solvers |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
