# 3-Node Shell Force Computation (`engine/source/elements/sh3n/`)

Computes internal forces for triangular (3-node) shell elements each time step. Three subdirectories provide different formulations:

## Subdirectories

| Directory | Formulation |
|-----------|-------------|
| `coque3n/` | Standard DKT (Discrete Kirchhoff Triangle) |
| `coquedk/` | Degenerate Kirchhoff variant |
| `coquedk6/` | Kirchhoff with 6-DOF (drilling rotation) |

## Key Files (`coque3n/` — primary formulation)

| File | Role |
|------|------|
| `c3coor3.F` | Update element co-rotational frame from current node positions |
| `c3coork3.F` | Compute element kinematics in local frame |
| `c3defo3.F` | Compute membrane and bending strain increments |
| `c3deri3.F` | Compute shape function derivatives for DKT |
| `c3fint3.F` | Compute internal force vector (membrane + bending) |
| `c3fint_reg.F` | Regularised internal force (hourglass-stabilised path) |
| `c3forc3.F` | Scatter forces/moments to global arrays |
| `c3fcum3.F` | Accumulate force contributions |
| `c3mcum3.F` | Accumulate moment contributions |
| `c3stra3.F` | Compute shell strains at integration points |
| `c3curv3.F` | Compute curvature from DKT interpolation |
| `c3evec3.F` | Compute element normal vector |
| `c3eoff.F` | Handle thickness offset |
| `c3dt3.F` | Compute element stable time step |
| `c3ke3.F` | Kinetic energy computation |
| `c3lke3.F` | Local kinetic energy |
| `c3pxpy3.F` | Stress resultant computation |
| `c3sumg3.F` | Global summation of contributions |
| `c3updt3.F` | Update element state after force computation |
| `c3be3.F` | Bending stiffness matrix contribution |
| `c3bilan.F` | Element energy balance |
| `temp3cg.F` | Temperature field interpolation at element centre |
| `therm3c.F` | Thermal strain contribution |
| `cssp2a11.F` | Stress smoothing for post-processing |

## DKT Formulation

The Discrete Kirchhoff Triangle (DKT) enforces zero transverse shear strain at discrete boundary points. This avoids shear locking for thin shells without requiring reduced integration. The element uses:
- 3 corner nodes, 6 DOF per node (3 translations + 3 rotations)
- Membrane: constant strain triangle (CST)
- Bending: DKT curvature interpolation with 9 bending DOF

## Related Documentation

- `starter/source/elements/sh3n/README.md` — 3-node shell initialisation
- `engine/source/elements/README.md` — parent elements directory overview
