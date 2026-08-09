# Cylindrical Joint Constraint (`engine/source/constraints/general/cyl_joint/`)

Implements cylindrical joint constraints: allows translation and rotation along/about a common axis while constraining the remaining 4 DOFs.

## Key Files

| File | Role |
|------|------|
| `cjoint.F` | Apply cylindrical joint kinematic constraints (velocity projection) |
| `telesc.F` | Telescopic joint variant: translation only along axis (no rotation) |
| `deallocate_joint.F` | Free joint constraint data structures at end of run |

## Algorithm

A cylindrical joint connects two nodes sharing a common axis. `cjoint.F` projects nodal velocities onto the allowed DOFs (axial translation + axial rotation) and removes the constrained components at each step. The force transfer is computed dually: constraint reaction forces are distributed back to both nodes.

`telesc.F` implements the pure-translation (telescopic) variant where the rotational DOF is also locked, leaving only axial sliding.

## Related Documentation

- `engine/source/constraints/general/README.md` — parent directory
- `engine/source/elements/joint/README.md` — element-based joint formulations
