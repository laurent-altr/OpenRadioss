# Constraints Subsystem

This subsystem enforces kinematic constraints that couple degrees of freedom or restrict motion. Constraints are fundamentally different from boundary conditions: they relate multiple nodes or bodies to each other, rather than prescribing absolute values.

## Directory Structure

```
constraints/
├── fxbody/        — Rigid body / rigid wall enforcement
├── general/       — General kinematic constraints (links, joints, RBE2/RBE3)
└── thermic/       — Thermal constraint enforcement
```

## Rigid Bodies and Rigid Walls (`fxbody/`)

Rigid bodies collect a set of nodes into a perfectly rigid object. The nodal motions are constrained to the 6 DOF (3 translation, 3 rotation) of the rigid body's reference node.

| File | Role |
|------|------|
| `fxbodv.F` | Rigid body velocity update from nodal velocities |
| `fxbodvp.F` | Rigid body velocity with penalty (contact) correction |
| `fxbodfp.F` | Rigid body force computation from constrained nodes |
| `fxbdispl.F` | Rigid body displacement update |
| `fxbsgmaj.F` | Main rigid body update loop |

The rigid body algorithm:
1. Gather nodal forces → compute resultant force and moment on rigid body
2. Integrate rigid body equations of motion (Newton-Euler)
3. Scatter rigid body accelerations back to all constrained nodes

### Rigid Walls

Rigid walls are a special case of `fxbody/`: they enforce a planar constraint. Their data structures are shared with `common_source/fxbody/`.

## General Constraints (`general/`)

| Subdirectory / File | Constraint type | Description |
|--------------------|----------------|-------------|
| `bcs/` | `/BCS` | Prescribed displacement/velocity DOF constraints |
| `cyl_joint/` | `/KJOINT2/CYL` | Cylindrical joint (allow rotation, restrict translation) |
| `impvel/` | `/IMPVEL` | Imposed velocity constraint |
| `rbe2/` | `/RBE2` | Rigid element type 2 (master-slave rigid link) |
| `kinini.F` | — | Kinematic constraint initialisation |

### RBE2

An RBE2 rigidly connects slave nodes to a master node. The slave node displacements and rotations are fully determined by the master. Used for bolted connections, spot-welds, and similar discrete rigid connections.

## Thermal Constraints (`thermic/`)

Enforces temperature compatibility constraints at thermal coupling interfaces — for example, ensuring temperature continuity between a solid body and its thermal boundary.

## Constraint Enforcement in the Time Loop

Constraints are applied in two places:
1. **Force step**: Constraint forces (reactions) are computed and added to the nodal force vector (handled in `fxbody/`, `general/`).
2. **Velocity step**: Constrained DOF velocities are set to match the constraint (handled in `assembly/` using the constraint flag arrays built here).

## Related Documentation

- `engine/source/boundary_conditions/README.md` — absolute DOF constraints (BCS/EBCS)
- `engine/source/assembly/README.md` — where constraint forces are applied
- `common_source/fxbody/` — shared rigid body data structures (starter + engine)
