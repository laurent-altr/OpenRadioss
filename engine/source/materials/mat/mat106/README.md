# LAW106 — Johnson-Cook with Non-Local Regularisation (`engine/source/materials/mat/mat106/`)

Elastic-plastic law combining Johnson-Cook power-law isotropic hardening,
JC logarithmic strain-rate sensitivity, and JC thermal softening, extended
with a non-local plastic strain increment (`dplanl`) for damage-regularisation
and temperature-dependent elastic moduli.

## Key Files

| File | Role |
|------|------|
| `sigeps106.F90` | Solid stress update: JC hardening σ_y = (A + B·ε_p^n)·(1+C·ln(ε̇/ε̇₀))·(1−T*^m), radial return, non-local plastic strain accumulation |
| `sigeps106c.F90` | Shell wrapper (called by `mulawc`) |

## Algorithm Notes

- Hardening: `hard = A + B·exp(n·ln(ε_p))` capped at `sigm`
- Strain-rate factor: `srdep = 1 + C·ln(1 + ε̇/ε̇₀)` (JC logarithmic form)
- Thermal softening from JC formula; temperature updated via Taylor-Quinney self-heating
- Non-local plastic increment `dplanl` is passed in/out for coupling with non-local damage models
- `mstrain_rate` (from `mat_share`) handles strain-rate filtering

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/materials/mat/mat002/README.md` — standard Johnson-Cook (LAW2, without non-local)
- `engine/source/materials/mat_share/README.md` — `mstrain_rate` shared utility
