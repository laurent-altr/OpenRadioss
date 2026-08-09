# General Failure Criterion Type 1 (`engine/source/materials/fail/gene1/`)

User-configurable generalised failure criterion combining equivalent stress, plastic strain, and energy sub-criteria with weighting factors.

## Key Files

| File | Role |
|------|------|
| `fail_gene1_c.F` | GENE1 criterion for solid elements |
| `fail_gene1_s.F` | GENE1 criterion for shell elements |
| `fail_gene1_b.F90` | GENE1 criterion for beam elements |
| `fail_gene1_ib.F90` | GENE1 criterion for implicit beam elements |

## Criterion

GENE1 combines multiple scalar indicators:

```
FI = w1 × (σ_eq/σ_max)^n1 + w2 × (ε_p/ε_p_max)^n2 + w3 × (W/W_max)^n3 ≥ 1
```

where `FI` is the failure index, `(w1, w2, w3)` are user weights, and exponents `(n1, n2, n3)` control the transition sharpness. Failure occurs when `FI ≥ 1`. This flexible formulation allows calibration from combined stress/strain/energy test data without committing to a specific physical model.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/energy/README.md` — energy-only criterion
