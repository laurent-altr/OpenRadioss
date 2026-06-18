# LAW44 — Elastic-Plastic for Concrete (Drucker-Prager) (`engine/source/materials/mat/mat044/`)

Concrete/geomaterial model using Drucker-Prager yield surface with
compression cap, tension cut-off, and progressive cracking.

## Key Files

| File | Role |
|------|------|
| `sigeps44.F` | Main stress update: DP yield + cap + tension |
| `sigeps44c.F` | Shell variant |
| `sigeps44p.F` | Plane-stress variant |
| `sigeps44pi.F` | Plane-stress incompressible |
| `sigeps44t.F` | Thermal variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
