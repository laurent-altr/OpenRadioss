# Rigid Wall Constraints (`engine/source/constraints/general/rwall/`)

Computes contact forces between nodes and rigid wall geometries (`/RWALL`).

## Key Files

| File | Role |
|------|------|
| `rgwall.F` | Main rigid wall contact loop: test all nodes against wall geometry, apply normal force |
| `rgwall_pen.F90` | Penalty-based rigid wall (alternative to kinematic) |
| `rgwalc.F` | Rigid wall contact: cylindrical wall geometry |
| `rgwal0.F` | Rigid wall initialisation for current time step |
| `rgwalp.F` | Rigid wall with friction — tangential force computation |
| `rgwals.F` | Rigid wall with sensor: deactivate when sensor triggers |
| `srw_imp.F` | Rigid wall for implicit solver |

## Rigid Wall Geometry Types

| Type | Shape |
|------|-------|
| FLAT | Infinite flat plane |
| OVAL | Cylindrical |
| SPHERE | Spherical |
| CONE | Conical |
| PARAL | Parallelogram (finite flat) |
| XFORM | Moving wall (arbitrary motion from function curve) |

## Algorithm

For each node `n`:
1. Compute signed distance `d` from node to wall surface
2. If `d < 0` (penetrating): apply normal force `F = K × d × n̂`
3. If friction: apply tangential force `F_t = μ × |F_n| × t̂`

The kinematic wall (default) sets penetrating node velocity to zero in the normal direction. The penalty wall (`rgwall_pen.F90`) adds a restoring force without modifying velocity directly.

## Related Documentation

- `engine/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/README.md` — rigid wall in overall constraints context
