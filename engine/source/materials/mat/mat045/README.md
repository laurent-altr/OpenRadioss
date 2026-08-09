# LAW45 — Hyperelastic (Rivlin Polynomial) (`engine/source/materials/mat/mat045/`)

Incompressible polynomial hyperelastic material (Rivlin series):
W = Σ Cᵢⱼ(I₁−3)ⁱ(I₂−3)ʲ. Generalises Mooney-Rivlin to higher order.

## Key Files

| File | Role |
|------|------|
| `sigeps45.F` | Main Cauchy stress from invariants I₁, I₂ |
| `sigeps45c.F` | Shell variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
