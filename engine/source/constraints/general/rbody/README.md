# Rigid Body (`engine/source/constraints/general/rbody/`)

Implements `/RBODY` rigid body kinematics: each rigid body moves as a single entity with 6 DOFs (3 translations, 3 rotations).

## Key Files

| File | Role |
|------|------|
| `rbyvit.F` | Update slave-node velocities from rigid-body velocity/angular velocity |
| `rbyac_imp.F` | Implicit rigid-body acceleration update |
| `rbyact.F` | Apply external forces/moments to rigid-body equations of motion |
| `rbycor.F` | Kinematic correction: enforce rigid-body constraints on displacements |
| `rbyfor.F` | Accumulate slave-node forces into rigid-body resultant force/moment |
| `rbyonf.F` | Project slave forces onto rigid-body DOFs (on-force) |
| `rbypid.F` | Rigid-body PID controller for prescribed motion |
| `srfvit.F` | Surface-velocity prescription for rigid body |
| `rgbodv.F` | Update rigid-body velocities from equations of motion |
| `rgbodfp.F` | Force/moment projection from slave nodes onto master DOFs |
| `rgbcor.F` | Rigid-body geometric correction (orientation update via quaternion) |
| `velrot_explicit.F90` | Explicit rotational velocity update using rotation matrix |
| `inepri.F` | Compute inertia tensor and principal axes of rigid body |
| `valpr.F` | Eigenvalue solver for inertia-tensor principal values |
| `rby_imp0.F` | Implicit rigid-body constraint at t=0 |
| `rby_impd.F` | Implicit rigid-body displacement update |

## Algorithm

At each explicit step:
1. `rbyfor.F` sums slave-node internal forces to get resultant `F_rb` and `M_rb`
2. `rgbodv.F` integrates: `v_rb += (F_rb/M) dt`, `ω_rb += I⁻¹ M_rb dt`
3. `rbyvit.F` distributes back to slave nodes: `v_slave = v_rb + ω_rb × r_slave`
4. `rgbcor.F` corrects the orientation quaternion for large rotations

## Related Documentation

- `engine/source/constraints/general/README.md` — parent constraints directory
- `engine/source/constraints/fxbody/README.md` — rigid wall (fixed rigid body)
