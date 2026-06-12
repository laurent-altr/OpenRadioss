# engine/source/constraints/

## Purpose
Kinematic constraints: rigid bodies, rigid walls, tied interfaces, rigid links,
cylindrical joints, RBE2/RBE3, and seatbelts. Constraints modify nodal forces
and velocities to enforce kinematic relationships beyond simple fixed BCs.

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `general/bcs/` | General nodal boundary conditions (translation/rotation lock flags `NODES%IN`) |
| `general/cyl_joint/` | Cylindrical joint: `TELESC` (telescoping), revolute/prismatic constraints |
| `general/impvel/` | Imposed velocity constraints (`IMPVEL*` family) |
| `general/rbe2/` | RBE2 rigid element interpolation (master-slave rigid links) |
| `general/rbe3/` | RBE3 force/moment interpolation (averaging constraints) |
| `general/rbody/` | Rigid body integrator: collects element forces → body resultant → updates body velocity/acceleration |
| `general/rlink/` | Rigid link (two-node kinematic constraint) |
| `general/rwall/` | Rigid wall: penalty / kinematic rigid-wall contact |
| `fxbody/` | Fixed-body velocity enforcement (see `boundary_conditions/fxbody/`) |
| `thermic/` | Thermal constraints: convection, radiation, fixed temperature/flux |

## Key concepts

**Rigid bodies** (`rbody/`): Each rigid body (`/RBODY`) aggregates nodal forces into
a 6-DOF resultant, integrates rigid-body equations of motion, then distributes the
body velocity back to slave nodes. Rigid bodies interact with contact interfaces and
can be connected via joints.

**`NODES%IN` fixity mask**: Each bit of `NODES%IN(I)` locks one DOF of node `I`.
Set by BCS routines during `RESOL_INIT`; read by `ACCELE` and velocity update.

**Rigid walls** (`rwall/`): Implement `/RWALL` — a geometric constraint surface
(plane, sphere, cylinder) that stops penetrating nodes. Uses a penalty or kinematic
approach; force exchange through `INTFOP8`.

## Call context in `RESOL`
Constraints are processed at multiple points:
- Initialization: `RESOL_INIT` sets `NODES%IN`, builds rigid-body slave lists
- Before force loops: rigid-wall sort (`INTFOP8`)
- After force assembly: rigid-body integration applies body motion to slave nodes
- After `ACCELE`: imposed-velocity constraints overwrite `NODES%V`

## Dependencies
- Called by: `RESOL`, `RESOL_INIT`
- MPI: `engine/source/mpi/kinematic_conditions/` (rigid-body force exchange)
