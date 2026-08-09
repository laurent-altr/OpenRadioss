# Rigid Joint Initialisation (`starter/source/elements/joint/rjoint/`)

Starter initialisation for rigid joint elements (revolute, prismatic, universal, cylindrical, spherical, planar joints).

## Key Files

| File | Role |
|------|------|
| `rini33.F` | Initialise TYPE33 revolute joint |
| `rini33_rb.F` | Initialise TYPE33 revolute joint (rigid body variant) |
| `rini45.F` | Initialise TYPE45 universal joint |
| `rini45_rb.F` | Initialise TYPE45 universal joint (rigid body variant) |
| `rini135_rb.F90` | Initialise TYPE135 multi-DOF joint (rigid body) |

## Description

Each `riniNN.F` routine sets up a specific joint type: builds the constraint matrix relating the two connected node pairs, computes the initial joint frame, and writes the joint parameters to the restart file. The engine enforces the joint kinematics via Lagrange multipliers each time step.

## Related Documentation

- `starter/source/elements/joint/README.md` — parent directory
- `engine/source/elements/joint/README.md` — engine joint enforcement
