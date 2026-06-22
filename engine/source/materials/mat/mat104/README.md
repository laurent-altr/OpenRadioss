# LAW104 — Progressive Composite Failure (Puck/Cuntze Newton/NICE) (`engine/source/materials/mat/mat104/`)

Composite progressive failure with Puck and Cuntze inter-fibre fracture
criteria; two numerical solvers: Newton (robust) and NICE (vectorised).
Both solid and shell variants with optional damage.

## Key Files

| File | Role |
|------|------|
| `sigeps104.F` / `sigeps104c.F` | Main / shell dispatcher |
| `mat104_nodam_newton.F` / `mat104_nodam_nice.F` | Undamaged solid: Newton / NICE |
| `mat104_ldam_newton.F` / `mat104_ldam_nice.F` | Linear damage solid: Newton / NICE |
| `mat104_nldam_newton.F` / `mat104_nldam_nice.F` | Nonlinear damage solid: Newton / NICE |
| `mat104c_nodam_newton.F` / `mat104c_nodam_nice.F` | Undamaged shell: Newton / NICE |
| `mat104c_ldam_newton.F` / `mat104c_ldam_nice.F` | Linear damage shell: Newton / NICE |
| `mat104c_nldam_newton.F` / `mat104c_nldam_nice.F` | Nonlinear damage shell: Newton / NICE |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
