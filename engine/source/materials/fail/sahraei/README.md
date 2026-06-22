# Sahraei Battery Failure Criterion (`engine/source/materials/fail/sahraei/`)

Mechanical failure criterion for lithium-ion battery cells under crush/indentation loading (Sahraei et al. 2012).

## Key Files

| File | Role |
|------|------|
| `fail_sahraei_s.F` | Sahraei failure for shell elements (battery cell casing) |

## Criterion

The Sahraei criterion characterises battery cell short-circuit onset under mechanical abuse:

```
(ε_1 / ε_1_max)² + (ε_2 / ε_2_max)² ≥ 1
```

where `ε_1, ε_2` are in-plane principal strains. The elliptical failure locus is calibrated from indentation, flat compression, and hemispherical punch tests on cell specimens. Failure indicates the onset of internal short circuit (ISC), which is the safety-critical event in battery crush simulations for EV crashworthiness.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/biquad/README.md` — generic bi-quadratic strain criterion
