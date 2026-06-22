# Lemaitre Failure (`starter/source/materials/fail/lemaitre/`)

Starter reader for /FAIL/LEMAITRE: Lemaitre continuum damage mechanics model with thermodynamic driving force.

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_lemaitre.F90` | Parse /FAIL/LEMAITRE card: S, s, ε_D (damage threshold) parameters |

## Description

Lemaitre CDM evolves damage as `Ḋ = (Y/S)^s × ε̇_p` where `Y = σ_eq²R_v/(2E(1-D)²)` is the strain energy release rate and `R_v` is the triaxiality function. Failure occurs when `D ≥ D_c` (critical damage).

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/lemaitre/README.md` — runtime damage evolution
