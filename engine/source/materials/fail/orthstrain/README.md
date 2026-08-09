# Orthotropic Maximum Strain Criterion (`engine/source/materials/fail/orthstrain/`)

Maximum strain criterion evaluated in material (orthotropic) axes for fiber-reinforced composites.

## Key Files

| File | Role |
|------|------|
| `fail_orthstrain_c.F` | Orthotropic max strain for solid elements |
| `fail_orthstrain_s.F` | Orthotropic max strain for shell elements |

## Criterion

Failure when any strain component in the material coordinate system exceeds its threshold:

```
|ε_L| ≥ ε_L_max    (fiber direction)
|ε_T| ≥ ε_T_max    (transverse direction)
|γ_LT| ≥ γ_LT_max  (shear)
```

Independent tension/compression limits in each direction. Simpler than Hashin but useful when only coupon test data (not strength ratios) is available. The material axes follow the element orientation update each step for large-deformation problems.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/orthbiquad/README.md` — quadratic extension
- `engine/source/materials/fail/hashin/README.md` — stress-based composite criterion
