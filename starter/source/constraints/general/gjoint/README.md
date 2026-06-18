# General Joint (`starter/source/constraints/general/gjoint/`)

Reads /GJOINT (general joint) definitions for kinematic constraints between rigid bodies.

## Key Files

| File | Role |
|------|------|
| `hm_read_gjoint.F` | Parse /GJOINT card: joint type, connected nodes, DOF constraints |

## Description

`/GJOINT` defines kinematic joints between rigid body nodes: revolute, prismatic, universal, cylindrical, screw, and user-defined joint types. The starter reads joint connectivity and constraint parameters, writing them to the restart file for the engine to enforce via Lagrange multipliers.

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `engine/source/tools/lagmul/README.md` — Lagrange multiplier enforcement
