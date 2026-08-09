# Tsai-Wu Failure Criterion (`engine/source/materials/fail/tsaiwu/`)

Implements the Tsai-Wu (1971) tensor polynomial failure criterion for anisotropic composites.

## Key Files

| File | Role |
|------|------|
| `fail_tsaiwu_c.F` | Tsai-Wu for solid elements |
| `fail_tsaiwu_s.F` | Tsai-Wu for shell elements |

## Criterion

The general quadratic failure criterion in stress space:

```
F_i σ_i + F_ij σ_i σ_j ≥ 1
```

Strength tensors `F_i` and `F_ij` are derived from uniaxial and biaxial test strengths `(X_T, X_C, Y_T, Y_C, S)`. The interaction term `F_12` is typically set to `−1/(2 √(X_T X_C Y_T Y_C))`. A single scalar reserve factor (failure index) indicates proximity to failure.

Tsai-Wu is a single-mode criterion (no distinction between fiber and matrix failure) suitable for woven fabrics and general orthotropic materials.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/tsaihill/README.md` — Tsai-Hill (simpler variant)
- `engine/source/materials/fail/hashin/README.md` — mode-separated composite criterion
