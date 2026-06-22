# Joint Element Force Computation (`engine/source/elements/joint/`)

Computes forces and moments for joint elements (`/RBODY`-based joints and `/KJOINT`).

## Key Files

| File | Role |
|------|------|
| `rgjoint.F` | Main joint force computation: evaluate joint stiffness/damping, compute reaction forces |
| `joint_block_stiffness.F` | Compute blocked-DOF stiffness contribution (penalty-based DOF locking) |
| `joint_elem_timestep.F` | Estimate stable time step for joint elements |
| `ranim33.F` | Write joint state to animation output |
| `rbilan33.F` | Joint element energy balance |
| `rcum33.F` / `rcum33p.F` | Accumulate joint forces into global arrays (serial / parallel) |
| `rdtime33.F` | Joint time-step computation |
| `rskew33.F` | Apply skew frame transformation to joint forces |
| `ruser33.F` | User-defined joint element (callback to user subroutine) |

## Joint Types

Joint elements connect two rigid bodies or two nodes with constrained relative motion. Supported DOF configurations:

| Joint Type | Free DOF |
|-----------|----------|
| Revolute | 1 rotation |
| Cylindrical | 1 rotation + 1 translation |
| Planar | 2 translations + 1 rotation |
| Ball-and-socket | 3 rotations |
| Universal | 2 rotations |
| Rigid | All DOF locked |
| Free | All DOF free |
| User | Custom (LAW user callback) |

## Force Computation

For each constrained DOF, a penalty spring force is computed:
```
F = -K × (current_displacement - equilibrium) - C × velocity
```
Free DOFs contribute zero constraint force. Stiffness `K` and damping `C` come from property TYPE33 (`/PROP/TYPE33`).

The `rgjoint.F` file also handles joint failure when a maximum force or displacement limit is exceeded.

## Related Documentation

- `engine/source/elements/README.md` — parent elements directory overview
- `engine/source/constraints/README.md` — rigid body and constraint management
