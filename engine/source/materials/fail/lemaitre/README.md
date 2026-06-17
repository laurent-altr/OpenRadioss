# Lemaitre Ductile Damage Criterion (`engine/source/materials/fail/lemaitre/`)

Implements the Lemaitre (1985) coupled damage mechanics model: a scalar damage variable `D` evolves with plastic strain and reduces element stiffness until failure.

## Key Files

| File | Role |
|------|------|
| `fail_lemaitre_c.F90` | Lemaitre damage for solid elements |
| `fail_lemaitre_s.F90` | Lemaitre damage for shell elements |

## Model

Damage evolution law:

```
Ḋ = (Y/S)^s × ε̇_p    (for ε_p > ε_D)
```

where `Y = σ_eq² / (2E(1-D)²) × R_v` is the damage energy release rate, `R_v` is a triaxiality factor, `ε_D` is the damage threshold, and `(S, s)` are material constants. The effective stress seen by plasticity is `σ_eff = σ/(1−D)`. At `D = D_c` (critical damage) the element fails and is deleted.

Unlike indicator-only criteria, Lemaitre modifies element stiffness throughout loading, making it a continuum damage mechanics (CDM) model.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/johnson_cook/README.md` — uncoupled JC criterion
