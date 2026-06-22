# Gurson Failure (`starter/source/materials/fail/gurson/`)

Starter reader for /FAIL/GURSON: Gurson-Tvergaard-Needleman (GTN) porous plasticity model with void growth and coalescence.

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_gurson.F` | Parse /FAIL/GURSON card: q1, q2, q3 parameters, nucleation, coalescence thresholds |

## Description

The GTN model tracks void volume fraction `f` and uses the modified yield function:
```
Φ = (σ_eq/σ_y)² + 2q₁f* cosh(3q₂σ_m/(2σ_y)) − (1 + q₃f*²) = 0
```
Failure occurs when `f* → 1/q₁`. The starter reads q parameters, initial void fraction, nucleation and critical void fractions.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/gurson/README.md` — runtime failure evaluation
