# Solid Connector (`engine/source/elements/solid/sconnect/`)

Solid connector elements: degenerate solid elements used as point-connector representations (spot welds, pins) in the solid mesh.

## Key Files

| File | Role |
|------|------|
| `sbilan43.F` | Force/energy balance for solid connector (4-3 topology) |
| `sconnect_off.F` | Element offset for solid connector |
| `scoor43.F` / `scoor431.F` | Coordinate mapping |
| `sdef43.F` | Deformation gradient |

## Description

Solid connectors are highly degenerate hexahedra collapsed to connect two surfaces or nodes, used as an alternative to spring elements for joining structural parts in solid-mesh models. `scoor43.F` handles the 4-3 (pyramid) topology created when two quad faces are connected at a node. Used for bolted flange models and interference-fit pin connections where full 3D stress transfer through the connector is needed.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/rivet/README.md` — rivet connector
- `engine/source/elements/spring/README.md` — spring connector alternative
