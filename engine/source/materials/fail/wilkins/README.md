# Wilkins Failure Criterion (`engine/source/materials/fail/wilkins/`)

Cumulative damage model for metals under dynamic loading (Wilkins 1980): combines hydrostatic stress and deviatoric stress asymmetry effects.

## Key Files

| File | Role |
|------|------|
| `fail_wilkins_c.F` | Wilkins criterion for solid elements |
| `fail_wilkins_s.F` | Wilkins criterion for shell elements |

## Criterion

Damage accumulates as:

```
D = ∫ W₁ × W₂ × dε_p
```

where:
- `W₁ = (1/(1 − p/p_lim))^α` accounts for hydrostatic tension (`p < 0` promotes fracture)
- `W₂ = (2 − A)^β` accounts for deviatoric stress asymmetry `A = max(s_i)/min(s_i)` among principal deviatoric stresses

Failure at `D ≥ D_crit`. The Wilkins model is especially suited to spall fracture and hypervelocity impact where hydrostatic tension drives void nucleation.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/spalling/README.md` — simpler spall criterion
- `engine/source/materials/fail/tuler_butcher/README.md` — cumulative impulse model
