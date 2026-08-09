# Solid Element Force Computation (`engine/source/elements/solid/`)

Computes internal forces for all 3D continuum (solid) elements each time step.

## Subdirectories

| Directory | Topology | Nodes | Notes |
|-----------|---------|-------|-------|
| `solide/` | Standard hexahedron | 8 | 1-point reduced integration, hourglass |
| `solide4/` | Tetrahedron | 4 | 1-point |
| `solide4_sfem/` | Tetrahedron (SFEM) | 4 | Smoothed FEM variant |
| `solide6z/` | Pentahedron (wedge) | 6 | 2-point |
| `solide8/` | Hex (full integration) | 8 | 2×2×2 Gauss |
| `solide8e/` | Hex (enhanced assumed strain) | 8 | EAS — incompatible modes |
| `solide8s/` | Hex (SFEM variant) | 8 | Smoothed strain |
| `solide8z/` | Hex (Z formulation) | 8 | Alternative hourglass |
| `solide10/` | Quadratic tet | 10 | 4-point |
| `solide20/` | Quadratic hex | 20 | 2×2×2 |
| `solidez/` | Hex (enhanced, z-type) | 8 | Combined EAS+hourglass |
| `sconnect/` | Surface connector (cohesive) | 8 | Traction-separation law |

## Common Force Computation Pattern

All solid formulations follow:
1. Compute deformation gradient `F` from current node coordinates
2. Compute strain measure (Green-Lagrange or incremental)
3. Call material law (`SIGEPS<NNN>`) to get updated stress
4. Compute internal force: `f_int = B^T σ V`
5. Scatter `f_int` to global force vector

The main difference between formulations is step 2 (strain computation) and the stabilisation method (hourglass vs full integration vs enhanced modes).

## `sconnect/` — Cohesive Zone

The surface connector models delamination in composites via a traction-separation law:
- Normal traction → mode I (opening)
- Shear traction → mode II (sliding)
- Failure when the energy release rate exceeds `Gc`

## Related Documentation

- `engine/source/elements/README.md` — parent elements overview
- `starter/source/elements/solid/README.md` — solid element initialisation
