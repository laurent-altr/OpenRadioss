# Johnson-Cook Failure (`starter/source/materials/fail/johnson_cook/`)

Starter reader for /FAIL/JOHNSON: Johnson-Cook ductile fracture criterion based on stress triaxiality, strain rate, and temperature.

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_johnson.F` | Parse /FAIL/JOHNSON card: d1–d5 constants, reference strain rate |

## Description

The Johnson-Cook failure strain is:
```
ε_f = [d1 + d2 exp(d3 σ*)] × [1 + d4 ln(ε̇*)] × [1 + d5 T*]
```
where `σ* = σ_m/σ_eq` (triaxiality), `ε̇* = ε̇/ε̇_0`, `T* = (T-T_r)/(T_m-T_r)`. Failure occurs when `D = ∫ dε_p/ε_f ≥ 1`.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/johnson_cook/README.md` — runtime failure evaluation
