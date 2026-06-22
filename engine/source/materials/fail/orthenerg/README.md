# Orthotropic Energy Failure Criterion (`engine/source/materials/fail/orthenerg/`)

Strain-energy based failure criterion for orthotropic materials, accumulating damage energy in material axes.

## Key Files

| File | Role |
|------|------|
| `fail_orthenerg_c.F` | Orthotropic energy criterion for solid elements |
| `fail_orthenerg_s.F` | Orthotropic energy criterion for shell elements |

## Criterion

Failure when specific strain energy in any material direction exceeds a threshold:

```
W_L = ∫ σ_L dε_L ≥ W_L_max     (fiber mode)
W_T = ∫ σ_T dε_T ≥ W_T_max     (matrix mode)
```

Energy thresholds are the area under the material stress-strain curve to failure (fracture toughness per unit volume). The energy accumulation is history-dependent, capturing loading/unloading path effects not captured by strain-only criteria. Used for composites under complex non-proportional loading.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/energy/README.md` — isotropic energy criterion
- `engine/source/materials/fail/orthstrain/README.md` — orthotropic strain criterion
