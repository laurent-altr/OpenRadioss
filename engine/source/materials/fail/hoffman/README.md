# Hoffman Failure Criterion (`engine/source/materials/fail/hoffman/`)

Implements the Hoffman (1967) failure criterion: a generalisation of Tsai-Hill that distinguishes tensile and compressive strengths.

## Key Files

| File | Role |
|------|------|
| `fail_hoffman_c.F` | Hoffman criterion for solid elements |
| `fail_hoffman_s.F` | Hoffman criterion for shell elements |

## Criterion

```
C1(σ_22−σ_33)² + C2(σ_33−σ_11)² + C3(σ_11−σ_22)²
  + C4 σ_11 + C5 σ_22 + C6 σ_33
  + C7 τ_23² + C8 τ_31² + C9 τ_12² ≥ 1
```

Coefficients `C1…C9` are derived from the nine independent strength values (tension, compression, shear in each direction). Unlike Tsai-Hill, Hoffman correctly handles materials that are stronger in compression than tension, common in ceramics and some composites.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/tsaiwu/README.md` — Tsai-Wu (general polynomial)
- `engine/source/materials/fail/tsaihill/README.md` — Tsai-Hill (symmetric strengths)
