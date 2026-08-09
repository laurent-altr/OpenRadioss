# RBE3 Constraint (`engine/source/constraints/general/rbe3/`)

Implements RBE3 interpolation elements: the master node motion is a weighted average of slave node motions (no stiffness added).

## Key Files

| File | Role |
|------|------|
| `rbe3f.F` | Force redistribution from master to slaves (standard RBE3) |
| `rbe3f_pen.F90` | Penalty-based RBE3 force redistribution |
| `rbe3v.F` | Velocity interpolation: master velocity from weighted-average of slaves |
| `rbe3pen_init.F90` | Initialise penalty RBE3 stiffness parameters |
| `rbe3_imp0.F` | Implicit solver constraint at initial step |

## Algorithm

Standard RBE3 (no stiffness):

```
x_master = Σ w_i × x_slave_i   (weighted centroid)
v_master = Σ w_i × v_slave_i
```

Forces at master are distributed back to slaves proportionally to weights `w_i`. The penalty variant (`rbe3f_pen.F90`) adds a small stiffness term to improve conditioning of the implicit solver when RBE3 master nodes participate in contact.

RBE3 is commonly used to apply loads or boundary conditions at a reference point without artificially stiffening the structure.

## Related Documentation

- `engine/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/rbe2/README.md` — RBE2 (rigid constraint)
- `common_source/modules/constraints/README.md` — rbe3_mod data structures
