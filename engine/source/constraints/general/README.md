# General Constraints (`engine/source/constraints/general/`)

Constraint enforcement routines for boundary conditions, rigid bodies, rigid walls, rigid links, and joints.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `bcs/` | Displacement/velocity boundary conditions (`/BCS`) |
| `rbody/` | Rigid body integration and constraint |
| `rbe2/` | RBE2 (rigid multi-point constraint — all DOF locked) |
| `rbe3/` | RBE3 (interpolation constraint — force/moment distributed) |
| `rwall/` | Rigid wall force computation |
| `rlink/` | Rigid link constraint (two-node rigid connector) |
| `cyl_joint/` | Cylindrical joint and telescopic joint |
| `impvel/` | Imposed velocity constraint |

## Top-Level File

| File | Role |
|------|------|
| `kinini.F` | Kinematic constraint initialisation: set up constraint tables at start of each time step |

## Constraint Evaluation Order

Each time step, constraints are applied in this order:
1. `bcs/` — zero or prescribe velocity DOF
2. `rbody/` — enforce rigid body kinematics
3. `rbe2/` / `rbe3/` — multi-point constraints
4. `rwall/` — rigid wall contact forces
5. `rlink/` — rigid connector
6. `impvel/` — imposed velocity (overrides other forces)

The order matters because later constraints can override earlier ones. Imposed velocities always win.

## Related Documentation

- `engine/source/constraints/README.md` — parent constraints directory
- `engine/source/constraints/fxbody/README.md` — low-level rigid body velocity application
