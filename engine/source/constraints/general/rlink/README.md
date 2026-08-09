# Rigid Link Constraint (`engine/source/constraints/general/rlink/`)

Implements `/RLINK` rigid-link multi-point constraints: linear combinations of nodal DOFs are held equal.

## Key Files

| File | Role |
|------|------|
| `rlink0.F` | Initialise rigid-link constraint data at start of step |
| `rlink1.F` | Apply rigid-link velocity constraints (1-DOF variant) |
| `rlink2.F` | Apply rigid-link velocity constraints (2-DOF variant) |
| `rlink10.F` | Generalised rigid-link: up to 10 slave DOFs tied to a single master DOF |

## Algorithm

A rigid link imposes:

```
v_slave = C × v_master
```

where `C` is a user-defined constraint coefficient. `rlink1.F`/`rlink2.F` handle 1- and 2-DOF cases optimised for common uses (e.g. tying a single translational DOF). `rlink10.F` supports the fully general case with up to 10 constrained DOF pairs, useful for complex kinematic linkages.

Reaction forces are projected back from the constraint to both master and slave nodes via virtual-work duality.

## Related Documentation

- `engine/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/rbe2/README.md` — RBE2 (rigid-body MPC)
