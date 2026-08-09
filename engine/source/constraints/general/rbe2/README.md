# RBE2 Constraint (`engine/source/constraints/general/rbe2/`)

Implements RBE2 rigid-body elements: all slave nodes share the motion of one master node (fully rigid multi-point constraint).

## Key Files

| File | Role |
|------|------|
| `rbe2f.F` | Distribute master-node force/moment to slave-node forces |
| `rbe2v.F` | Apply master-node velocity/angular velocity to slave nodes |
| `rbe2_imp0.F` | Implicit solver constraint at initial step |

## Algorithm

RBE2 constrains slave-node velocities rigidly to the master:

```
v_slave = v_master + ω_master × (x_slave − x_master)
```

`rbe2v.F` enforces this each step. `rbe2f.F` performs the dual (virtual-work) projection: slave forces are condensed into an equivalent force + moment at the master node, then the master participates in the global equations of motion.

Unlike `/RBODY`, RBE2 master and slave nodes may belong to deformable parts; the constraint is purely kinematic (no mass or inertia is added).

## Related Documentation

- `engine/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/rbe3/README.md` — RBE3 (averaging constraint)
- `engine/source/constraints/general/rbody/README.md` — full rigid body
