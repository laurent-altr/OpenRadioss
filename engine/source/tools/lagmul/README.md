# Engine Lagrange Multiplier Tools (`engine/source/tools/lagmul/`)

Applies Lagrange multiplier constraint forces for joints, gear constraints, and rack-and-pinion mechanisms.

## Key Files

| File | Role |
|------|------|
| `cholfact.F` | Cholesky factorisation for small constraint system |
| `gjnt_diff.F` | Differential gear joint constraint |
| `gjnt_gear.F` | Gear joint constraint (fixed ratio coupling) |
| `gjnt_rack.F` | Rack-and-pinion constraint |
| `lag_anith.F` | Lagrange multiplier anti-hourglassing constraint |

## Algorithm

Gear and rack/pinion constraints couple rotational DOFs: `ω_2 = r × ω_1` (gear ratio `r`). `gjnt_gear.F` and `gjnt_rack.F` form the constraint Jacobian and use a Lagrange multiplier to enforce the velocity constraint. `cholfact.F` performs local 2×2 Cholesky factorisation for the constraint system solve at each step. `lag_anith.F` adds anti-hourglassing multipliers for elements near constrained nodes.

## Related Documentation

- `engine/source/tools/README.md` — parent tools directory
- `starter/source/tools/lagmul/README.md` — Lagrange multiplier definition in starter
