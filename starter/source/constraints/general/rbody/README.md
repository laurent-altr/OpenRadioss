# Rigid Body (`starter/source/constraints/general/rbody/`)

Reads and initialises /RBODY: rigid body definitions that lump a set of deformable nodes into a single rigid body with 6 DOF.

## Key Files

| File | Role |
|------|------|
| `hm_preread_rbody.F` | Pre-read: count /RBODY blocks, allocate arrays |
| `hm_read_rbody.F` | Parse /RBODY card: main node, node set, inertia properties |
| `hm_read_rbody_lagmul.F` | Parse Lagrange multiplier rigid body variant |
| `preread_rbody_lagmul.F` | Pre-read for Lagrange multiplier variant |
| `inirby.F` | Initialise rigid body: compute mass, centre of gravity, inertia tensor |
| `inisrf.F` | Initialise rigid body surface (for contact) |
| `checkrby.F` | Validate rigid body: check node uniqueness, no shared DOFs |
| `chbas.F` | Compute rigid body basis vectors (principal axes) |
| `hierarchy_rbody.F90` | Resolve rigid body hierarchy (nested rigid bodies) |
| `rbody_part_modif.F90` | Modify part assignments for rigid body elements |
| `ifrbody_off.F90` | Handle rigid body deactivation (sensor-triggered) |
| `spmdset.F` | Set SPMD domain flags for rigid body nodes |

## Description

`hm_read_rbody.F` reads the rigid body definition: the main (reference) node, the slave node set, and optional inertia override. `inirby.F` computes the total mass and inertia tensor from the slave node masses, and places the centre of gravity. `hierarchy_rbody.F90` resolves cases where rigid bodies are nested (sub-body within a rigid body). The data is written to the restart file for the engine to enforce kinematic rigidity each step.

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/rbody/README.md` — engine-side rigid body integration
