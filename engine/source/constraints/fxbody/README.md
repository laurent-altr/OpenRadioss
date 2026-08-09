# Rigid Body Force Application (`engine/source/constraints/fxbody/`)

Computes and applies rigid body forces and motion constraints each time step.

## Key Files

| File | Role |
|------|------|
| `fxbodv.F` | Apply rigid body velocity: enforce all slave nodes to follow master node velocity |
| `fxbodvp.F` | Parallel (OpenMP) version of `fxbodv.F` |
| `fxbodfp.F` | Gather forces from slave nodes to rigid body master node (force assembly) |
| `fxbdispl.F` | Apply rigid body displacement constraint (position correction) |
| `fxbsgmaj.F` | Compute rigid body major axis moment of inertia update |
| `fxbsys.F` | Update rigid body local coordinate system |
| `fxbyfor.F` | Compute rigid body resultant force and torque from slave forces |
| `fxbyvit.F` | Compute rigid body velocity update from resultant force/torque |
| `fxbypid.F` | PID controller for rigid body motion (prescribed motion control) |
| `fxgrvcor.F` | Gravity correction for rigid body (subtract gravity from constraint forces) |

## Rigid Body Algorithm

At each time step:

```
1. fxbyfor.F  — gather slave forces → resultant F, M at CG
2. fxbyvit.F  — integrate: F/m → a_CG, M/I → α (angular acceleration)
3. fxbodv.F   — enforce: v_slave = v_CG + ω × r_slave
4. fxbdispl.F — correct position: x_slave = x_CG + R × x_local
```

All slave nodes share the rigid body's rigid motion — they have no independent degrees of freedom.

## Related Documentation

- `engine/source/constraints/README.md` — parent constraints overview
- `engine/source/constraints/general/rbody/` — rigid body state (inepri.F, rbyact.F)
