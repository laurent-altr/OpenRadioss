# Constraint Modules (`common_source/modules/constraints/`)

Fortran 90 modules defining data types for kinematic constraints. Shared between starter and engine.

## Modules

| File | Module | Contents |
|------|--------|---------|
| `rbe3_mod.F90` | `RBE3_MOD` | RBE3 constraint data: master node, slave node weights, constrained DOF mask |
| `rwall_mod.F90` | `RWALL_MOD` | Rigid wall data: geometry type, position/orientation, velocity function, friction |

## RBE3 Module

RBE3 is an interpolation constraint (unlike RBE2 which is rigid). The master node motion is the weighted average of slave node motions. `RBE3_MOD` stores:
- Master node ID
- Slave node list and per-node weight factors
- Which DOF of the master are constrained by interpolation
- Independent DOF (not interpolated)

This is used for attaching a load cell or sensor node to a surface without artificially stiffening the surface.

## RWALL_MOD

Rigid wall definitions:
- Geometry type code (flat, cylindrical, spherical, etc.)
- Normal direction and reference point
- Velocity function reference (for moving walls)
- Friction coefficient and penalty factor
- Sensor ID for activation/deactivation

## Related Documentation

- `common_source/modules/README.md` — parent modules directory
- `engine/source/constraints/README.md` — constraint implementation
- `engine/source/constraints/general/rwall/README.md` — rigid wall contact algorithm
