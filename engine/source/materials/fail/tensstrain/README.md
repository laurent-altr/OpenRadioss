# Tensile Strain Failure Criterion (`engine/source/materials/fail/tensstrain/`)

Failure criterion based on maximum principal tensile strain, with optional additional shear strain check.

## Key Files

| File | Role |
|------|------|
| `fail_tensstrain_c.F` | Tensile strain failure for solid elements |
| `fail_tensstrain_s.F` | Tensile strain failure for shell elements |
| `fail_tensstrain_b.F` | Tensile strain failure for beam elements |
| `fail_tensstrain_ib.F` | Tensile strain failure for implicit beam elements |

## Criterion

Failure when the maximum principal tensile strain exceeds a threshold:

```
max(ε_1, 0) ≥ ε_tens_max
```

Optionally combined with an engineering shear strain check:

```
max(γ_12, γ_23, γ_31) ≥ γ_max
```

The Macaulay bracket (positive-only) ensures compressive strains do not trigger failure. Simpler than the bi-quadratic criterion but appropriate for brittle materials (concrete, ceramics, glass) where tensile fracture initiates normal to the maximum principal tensile stress.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/max_strain/README.md` — multi-direction max strain
- `engine/source/materials/fail/spalling/README.md` — pressure-based brittle failure
