# 2D Solid Elements (`engine/source/elements/solid_2d/`)

Computes internal forces for 2D plane strain/stress and axisymmetric solid elements.

## Subdirectories

| Directory | Element | Notes |
|-----------|---------|-------|
| `quad/` | 4-node quadrilateral 2D solid (1-point) | Plane strain/stress |
| `quad4/` | 4-node quadrilateral (full 2×2 integration) | Alternative |

## 2D vs 3D Solids

2D solids are used for plane strain (`εzz = 0`), plane stress (`σzz = 0`), or axisymmetric problems where the geometry is constant in the third direction. They are significantly cheaper than 3D solids and are used for:
- Cross-sectional analysis (extrusion, forming)
- Axisymmetric structures (cylinders, spheres)
- Initial model development with full 3D follow-up

The force computation is analogous to 3D solids but with 2D (or 2D+1 axisymmetric) strain and stress tensors.

## Related Documentation

- `engine/source/elements/solid/README.md` — 3D solid elements
- `engine/source/elements/README.md` — parent elements overview
