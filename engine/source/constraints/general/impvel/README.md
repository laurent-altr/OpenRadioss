# Imposed Velocity Constraint (`engine/source/constraints/general/impvel/`)

Enforces prescribed velocity boundary conditions (`/IMPVEL`) on node groups each time step.

## Key Files

| File | Role |
|------|------|
| `fixvel.F` | Apply imposed translational/rotational velocities to nodes |
| `fixfingeo.F` | Imposed velocity with follower frame (velocity rotates with node local frame) |
| `fv_imp0.F` | Implicit solver imposed-velocity constraint at t = 0 |

## Algorithm

`fixvel.F` directly overwrites the nodal velocity components specified in the constraint definition:

```fortran
V(node, dof) = fscale(t) * v_prescribed
```

where `fscale(t) = funct(t) × user_scale`. If a node belongs to both a rigid body and an `/IMPVEL`, the rigid-body velocity is overridden for the constrained DOFs.

`fixfingeo.F` handles the follower-force case where the prescribed velocity direction is expressed in a moving local frame (SKEW/MOV): the velocity vector is rotated each step by the current skew orientation.

## Related Documentation

- `engine/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/bcs/README.md` — fixed displacement BCS
- `starter/source/loads/README.md` — imposed velocity input
