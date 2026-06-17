# Starter Interfaces (Contact) Subsystem

This subsystem reads and validates all `/INTER/TYPE<N>` contact interface keywords and initialises the contact data structures written to the restart file.

## Role

The starter's `interfaces/` directory is the **parsing and initialisation** counterpart to the engine's `engine/source/interfaces/` directory (which performs the runtime contact detection and force computation).

For each interface type, the starter:
1. Reads keyword parameters (gap, penalty, friction, surface definitions)
2. Identifies and validates secondary/primary surface entities
3. Builds the interface buffer (`intbuf_struct`) with the initial candidate configuration
4. Performs geometric checks (initial penetration detection, segment normals)
5. Writes all interface data to the restart file

## Directory Structure

```
interfaces/
├── int01/–int25/  — Per-type interface parsers (TYPE1 through TYPE25)
├── friction/      — Friction model parameter reading
├── intbuf/        — Interface buffer initialisation
├── inter2d1/      — 2D interface initialisation
├── inter3d1/      — 3D interface initialisation
├── interf1/       — Common interface utilities
└── reader/        — Alternative input format reading (HyperMesh, CFG)
```

## Interface Types Supported

| Subdirectory | Contact type | Description |
|-------------|-------------|-------------|
| `int01/` | TYPE1 | 1D tied interface |
| `int02/` | TYPE2 | Node-to-node tied |
| `int03/` | TYPE3 | Node-to-segment |
| `int05/` | TYPE5 | Node-to-surface (edge) |
| `int06/` | TYPE6 | Surface-to-surface (symmetric) |
| `int07/` | TYPE7 | Node-to-surface penalty (most common) |
| `int08/` | TYPE8 | Generalised tied |
| `int09/` | TYPE9 | Node-to-surface tied |
| `int10/` | TYPE10 | Tied with failure |
| `int11/` | TYPE11 | Edge-to-edge |
| `int12/` | TYPE12 | Surface-to-surface penalty |
| `int14/` | TYPE14 | Symmetric penalty |
| `int15/` | TYPE15 | Tied with offset |
| `int16/` | TYPE16 | Rigid body contact |
| `int17/` | TYPE17 | Master-slave penalty |
| `int18/` | TYPE18 | General tied (spotwelds) |
| `int20/` | TYPE20 | Rigid wall (velocity) |
| `int21/` | TYPE21 | Rigid wall (sensor) |
| `int22/` | TYPE22 | Fluid-structure |
| `int23/` | TYPE23 | SPH-to-surface |
| `int24/` | TYPE24 | Edge-to-surface |
| `int25/` | TYPE25 | General segment-based |

## Friction Models (`friction/`)

Reads friction law parameters referenced by interface definitions:
- Coulomb (static/dynamic coefficient)
- Viscous friction
- Tabulated friction (coefficient vs. velocity)

## Related Documentation

- `engine/source/interfaces/README.md` — runtime contact algorithm (engine)
- `common_source/interf/README.md` — shared interface buffer data structure
