# Truss Property (`starter/source/properties/truss/`)

Reads truss element section properties (`/PROP/TRUSS`).

## Key Files

| File | Role |
|------|------|
| `hm_read_prop02.F` | Read truss property: cross-sectional area, optional pre-tension |

## Description

Truss elements carry only axial force (no bending or torsion). The property card specifies:
- Cross-sectional area `A` (used to compute axial stress from axial force)
- Optional pre-stress or pre-strain for pre-tensioned cables or bolts

Trusses are commonly used for cables, rods, and reinforcement bars in concrete models (embedded rebar). Unlike beam elements, trusses have only 3 translational DOFs per node.

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `engine/source/elements/truss/README.md` — truss element in engine
