# TYPE10 Interface — Rigid Wall (`starter/source/interfaces/int10/`)

Starter reader for /INTER/TYPE10: rigid wall contact (infinite plane, cylinder, or sphere) acting as a master surface.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type10.F` | Parse /INTER/TYPE10 card parameters: geometry, motion, restitution |

## Description

TYPE10 defines a rigid wall (geometric primitive: plane, sphere, or cylinder) that slave nodes cannot penetrate. The wall can be fixed, moving (prescribed velocity), or force-driven. The starter reads the geometry definition, slave node set, friction coefficient, and optional sensor-triggered activation.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `engine/source/constraints/general/README.md` — rigid body constraints
