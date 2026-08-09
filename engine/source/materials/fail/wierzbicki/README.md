# Wierzbicki / Hosford-Coulomb Fracture Criterion (`engine/source/materials/fail/wierzbicki/`)

Extended ductile fracture model in `(triaxiality, Lode angle)` space using the Hosford-Coulomb fracture locus.

## Key Files

| File | Role |
|------|------|
| `fail_wierzbicki_c.F` | Wierzbicki criterion for solid elements |
| `fail_wierzbicki_s.F` | Wierzbicki criterion for shell elements |

## Criterion

The Hosford-Coulomb fracture locus defines the critical strain as a function of stress triaxiality `η` and Lode angle `θ̄`:

```
ε_f(η, θ̄) = b × [a + √3/(2 − √3) × (sin(πθ̄/6) + c)] × [(1 + c₁²)^(1/n) ...]
```

Parameters `(a, b, c, n)` are calibrated from notched tensile, shear, and plane-strain tests. Damage accumulates as:

```
D = ∫ dε_p / ε_f(η(t), θ̄(t))
```

Failure at `D = 1`. This criterion correctly predicts failure mode transitions between tension-dominated (void growth) and shear-dominated (shear band) fracture paths.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/tabulated/README.md` — tabulated fracture locus
- `engine/source/materials/fail/rtcl/README.md` — Rice-Tracey/CL combined
