# Special Connector Property (`starter/source/properties/xelem/`)

Reads properties for special connector elements (XELEM types 28–31): ball joints, sliding joints, and kinematic constraints represented as single-element connectors.

## Key Files

| File | Role |
|------|------|
| `hm_read_prop28.F` | Read XELEM property: connector stiffness and DOF configuration |

## Description

XELEM special connectors (types 28–31) are multi-DOF connector elements connecting nodes with configurable stiffness in each translational and rotational degree of freedom. The property card specifies:
- Axial, transverse, and rotational stiffness values or spring curves
- Damping coefficients
- Gap/pre-load in each DOF

Used for hinge joints, ball-and-socket joints, sliding doors, and similar kinematic linkages in vehicle dynamic models and structural assemblies.

## Related Documentation

- `starter/source/properties/README.md` — parent directory
- `engine/source/elements/xelem/README.md` — XELEM element in engine
