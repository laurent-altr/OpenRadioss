# Fixed Body (Rigid Wall/Body) Shared Data (common_source/fxbody)

This directory contains the shared initialisation routine for rigid body / rigid wall data structures, used by both the starter (to build the structures) and the engine (to initialise them at engine startup from restart data).

## File

| File | Role |
|------|------|
| `fxbvini.F` | `FXBVINI` — initialise velocity, geometry, and inertia data for all rigid bodies and rigid walls |

## What `FXBVINI` Does

At startup, `FXBVINI` initialises the rigid body kinematic state:
- Sets initial translational and angular velocities from restart data
- Computes the inertia tensor from the mass distribution of the constrained nodes
- Sets up the rotation matrix from the initial orientation

This routine runs in both starter and engine; in the starter it prepares the initial state from the keyword deck, in the engine it reads from the restart file.

## Rigid Body vs. Rigid Wall

- **Rigid body** (`/RBODY`): a deformable mesh region replaced by a 6-DOF rigid object. All nodes move as a single rigid body.
- **Rigid wall** (`/RWALL`, `/WALL`): a fixed or moving planar constraint that blocks node penetration.

Both are treated by the `fxbody/` data structures and share the `FXBVINI` initialisation. The full enforcement logic is in `engine/source/constraints/fxbody/`.

## Related Documentation

- `engine/source/constraints/README.md` — runtime enforcement of rigid body constraints
- `common_source/README.md` — overview of shared common_source utilities
