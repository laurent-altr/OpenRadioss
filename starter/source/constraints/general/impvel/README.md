# Imposed Velocity / Displacement / Acceleration (`starter/source/constraints/general/impvel/`)

Reads imposed kinematic conditions: prescribed velocity, displacement, and acceleration as time functions on node sets.

## Key Files

| File | Role |
|------|------|
| `hm_preread_impvel.F` | Pre-read: count /IMPVEL blocks |
| `hm_preread_impvel0.F` | Pre-read for zero-velocity variant |
| `hm_preread_impacc.F` | Pre-read: count /IMPACC blocks |
| `hm_preread_impdisp.F` | Pre-read: count /IMPDISP blocks |
| `hm_read_impvel.F` | Parse /IMPVEL: velocity components, function IDs, DOF mask |
| `hm_read_impacc.F` | Parse /IMPACC: acceleration components, function IDs |
| `read_impvel.F` | Main imposed velocity reader (legacy format) |
| `read_impvel_fgeo.F` | Read imposed velocity in geometric frame |
| `read_impvel_lagmul.F` | Read Lagrange multiplier imposed velocity |
| `read_impdisp.F` | Read imposed displacement |
| `read_impdisp_fgeo.F` | Read imposed displacement in geometric frame |

## Description

Imposed velocity constrains selected DOFs of a node set to follow a time function. Unlike fixed BCS (zero DOF), the magnitude can vary. `read_impvel_lagmul.F` uses a Lagrange multiplier formulation for accuracy at large imposed velocities. `/IMPDISP` imposes incremental or total displacement, converted internally to velocity.

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `engine/source/constraints/general/impvel/README.md` — engine-side enforcement
