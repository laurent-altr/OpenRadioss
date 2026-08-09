# Starter Constraints Subsystem

This subsystem reads and initialises all kinematic constraint definitions: rigid bodies, rigid walls, joints, rigid links, and tied DOF constraints.

## Directory Structure

```
constraints/
├── fxbody/        — Rigid body and rigid wall initialisation
├── general/       — General kinematic constraints
│   ├── bcs/       — BCS prescribed DOF constraints
│   ├── cyl_joint/ — Cylindrical joint
│   ├── gjoint/    — General joint
│   ├── impvel/    — Imposed velocity
│   ├── merge/     — Merge constraint (tied nodes)
│   └── rbe2/      — RBE2 rigid element
├── rigidlink/     — Rigid link constraints
├── ale/           — ALE-specific constraints
└── sph/           — SPH particle constraints
```

Key files at the top level:
- `kinini.F` — kinematic constraint initialisation dispatcher
- `kinset.F` — set-based constraint handling
- `kinchk.F` — constraint consistency checks

## Rigid Bodies (`fxbody/`)

Reads `/RBODY` definitions:
- Node group that forms the rigid body
- Reference node (master DOF)
- Initial velocity / angular velocity
- Inertia tensor (computed from node distribution or specified)

Also handles `/RWALL` (rigid wall) definitions.

## General Constraints (`general/`)

| Subdirectory | Keyword | Description |
|-------------|---------|-------------|
| `bcs/` | `/BCS` | Prescribed DOF (fix translation/rotation axes) |
| `cyl_joint/` | `/KJOINT2/CYL` | Cylindrical joint between two parts |
| `gjoint/` | `/GJOINT` | General joint (tabulated force-displacement) |
| `impvel/` | `/IMPVEL` | Time-varying imposed velocity on node group |
| `merge/` | — | Merged (permanently tied) node pairs |
| `rbe2/` | `/RBE2` | Rigid element: slave nodes follow master |

## Rigid Links (`rigidlink/`)

`/RLINK` connects a set of slave nodes rigidly to a master node, similar to RBE2 but without the full 6-DOF rigid body formulation. Simpler and cheaper for spot-weld modelling.

## ALE Constraints (`ale/`)

ALE-specific kinematic constraints:
- Mesh velocity prescription at ALE boundaries
- Fixed ALE boundary nodes (mesh velocity = 0 at rigid walls)

## Constraint Compatibility Checks (`kinchk.F`)

After reading all constraints, the starter checks for:
- Conflicting constraints on the same node
- Nodes belonging to multiple incompatible rigid bodies
- Overconstrained DOFs

Warnings or errors are issued for incompatible combinations.

## Related Documentation

- `engine/source/constraints/README.md` — runtime constraint enforcement
- `common_source/fxbody/README.md` — shared rigid body/wall data structures
