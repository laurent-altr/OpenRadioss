# 2D Quad (4-node) Plane Solid (`engine/source/elements/solid_2d/quad/`)

4-node quadrilateral 2D solid element for plane strain, plane stress, or axisymmetric analysis.

## Key Files

| File | Role |
|------|------|
| `qcoor2.F` | 2D coordinate mapping and Jacobian |
| `qdefo2.F` | 2D strain-displacement matrix |
| `qfint2.F` | 2D internal force with 1-point quadrature |
| `qforc2.F` | Force assembly for 2D quad |
| `qbilan.F` | Force/energy balance |
| `qdlen2.F` | Element edge length for DT estimate |
| `qcumu2.F` / `qcumu2p.F` | Cumulative strain (standard / plane stress) |

## Formulation

1-point reduced integration with hourglass stabilisation. Supports all 2D analysis modes:
- **Plane strain**: `ε_33 = 0`, full in-plane stress state
- **Plane stress**: `σ_33 = 0`, thin structure in-plane
- **Axisymmetric**: hoop stress `σ_θθ` from radial displacement

## Related Documentation

- `engine/source/elements/solid_2d/README.md` — parent 2D solid directory
- `engine/source/elements/solid_2d/quad4/README.md` — enhanced 4-node variant
