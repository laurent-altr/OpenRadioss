# LAW90 — Elastic-Plastic with Polynomial EOS (`engine/source/materials/mat/mat090/`)

Elastic-plastic material with polynomial (Mie-Grüneisen variant) equation of
state. Suitable for metals under shock: p = f(μ, e) with Hugoniot correction.

## Key Files

| File | Role |
|------|------|
| `sigeps90.F` | Main stress update: J2 plasticity + polynomial EOS pressure |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
