# Tuler-Butcher Fracture Criterion (`engine/source/materials/fail/tuler_butcher/`)

Cumulative impulse fracture criterion for dynamic (spall) failure in metals under stress-wave loading (Tuler-Butcher 1968).

## Key Files

| File | Role |
|------|------|
| `fail_tbutcher_c.F` | Tuler-Butcher for solid elements |
| `fail_tbutcher_s.F` | Tuler-Butcher for shell elements |
| `fail_tbutcher_xfem.F` | Tuler-Butcher driving XFEM crack for spall plane |

## Criterion

Damage accumulated only when tensile stress exceeds a threshold `σ_0`:

```
K = ∫ ⟨σ − σ_0⟩^λ dt ≥ K_crit
```

The time integral rather than strain integral makes this criterion rate-sensitive: short intense pulses cause less damage than sustained tension at the same peak stress. Calibrated from Hopkinson-bar spall tests. The `_xfem` variant triggers a planar spall crack when the criterion is met.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/spalling/README.md` — pressure-based spall
- `engine/source/materials/fail/wilkins/README.md` — Wilkins dynamic fracture model
