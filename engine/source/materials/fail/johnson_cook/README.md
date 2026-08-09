# Johnson-Cook Failure Criterion (`engine/source/materials/fail/johnson_cook/`)

Implements the Johnson-Cook ductile fracture criterion (equivalent plastic strain at failure as a function of stress triaxiality, strain rate, and temperature).

## Key Files

| File | Role |
|------|------|
| `fail_johnson.F` | Standard Johnson-Cook failure: equivalent plastic strain threshold |
| `fail_johnson_b.F` | Johnson-Cook failure for beam elements |
| `fail_johnson_c.F` | Johnson-Cook failure for solid elements (3D stress state) |
| `fail_johnson_ib.F` | Johnson-Cook failure for implicit beam elements |
| `fail_johnson_xfem.F` | Johnson-Cook failure driving XFEM crack propagation |

## Criterion

The critical failure strain is:

```
ε_f = [d1 + d2 exp(d3 σ*)] × [1 + d4 ln(ε̇*)] × [1 + d5 T*]
```

where `σ* = p/σ_eq` (stress triaxiality), `ε̇*` is normalised strain rate, and `T*` is homologous temperature. Failure occurs when accumulated equivalent plastic strain reaches `ε_f`. The `_xfem` variant feeds the damage indicator to the XFEM level-set for explicit crack growth.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/elements/xfem/README.md` — XFEM crack propagation
