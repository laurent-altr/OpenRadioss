# LAW34 — Fabric (`engine/source/materials/mat/mat034/`)

Woven-fabric material: tension-only fibers in warp and weft directions,
nonlinear shear, compressive fold-compression, and rate effects.

## Key Files

| File | Role |
|------|------|
| `sigeps34.F` | Main stress update: warp/weft tension, shear |
| `sigeps34c.F` | Shell variant |
| `sigeps34pi.F` | Plane-stress incompressible variant |
| `sigeps34t.F` | Tabulated-input variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
