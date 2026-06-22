# Bi-Quadratic Failure Criterion (`engine/source/materials/fail/biquad/`)

Implements a general quadratic failure surface in engineering strain space (isotropic materials).

## Key Files

| File | Role |
|------|------|
| `fail_biquad_c.F` | Bi-quadratic failure for solid elements |
| `fail_biquad_s.F` | Bi-quadratic failure for shell elements |
| `fail_biquad_b.F` | Bi-quadratic failure for beam elements |
| `fail_biquad_ib.F` | Bi-quadratic failure for implicit beam elements |

## Criterion

The failure surface is an ellipsoid in `(ε_1, ε_2)` principal strain space:

```
(ε_1/a)² + (ε_2/b)² + c ε_1 ε_2 ≥ 1
```

Parameters `a`, `b`, `c` are calibrated from uniaxial, biaxial, and shear tests. This offers more flexibility than the simple maximum strain criterion while avoiding full tensor-polynomial complexity. Commonly used for metals and thermoplastics.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/max_strain/README.md` — simpler maximum strain
- `engine/source/materials/fail/orthbiquad/README.md` — orthotropic variant
