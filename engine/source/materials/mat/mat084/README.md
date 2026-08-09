# LAW84 — Modified Gurson-Tvergaard-Needleman (GTN) (`engine/source/materials/mat/mat084/`)

Porous plastic model with GTN yield function: Φ = (σ_eq/σ_y)² + 2q₁f* cosh(q₂ σ_kk / 2σ_y) − (1+q₃f*²) = 0.
Void nucleation, growth, and coalescence with modified f* at failure.

## Key Files

| File | Role |
|------|------|
| `sigeps84.F` | Main stress update: GTN return mapping + void evolution |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
