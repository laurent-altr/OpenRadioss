# Tsai-Hill Failure Criterion (`engine/source/materials/fail/tsaihill/`)

Implements the Tsai-Hill failure criterion: a Hill-type quadratic polynomial in stress components for orthotropic composites.

## Key Files

| File | Role |
|------|------|
| `fail_tsaihill_c.F` | Tsai-Hill for solid elements |
| `fail_tsaihill_s.F` | Tsai-Hill for shell elements |

## Criterion

```
(σ_11/X)² − (σ_11 σ_22/X²) + (σ_22/Y)² + (τ_12/S)² ≥ 1
```

where `X`, `Y` are the longitudinal and transverse strengths (same value used for tension and compression), and `S` is the shear strength. Tsai-Hill does not distinguish tension from compression and is less accurate than Tsai-Wu for composites with asymmetric strength, but is simpler and computationally inexpensive.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/tsaiwu/README.md` — Tsai-Wu (tension/compression distinction)
- `engine/source/materials/fail/hoffman/README.md` — Hoffman (asymmetric strengths)
