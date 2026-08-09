# Thick-Shell Property (`starter/source/properties/thickshell/`)

Reads thick-shell element section properties (also called continuum shells or layered solid-shell).

## Key Files

| File | Role |
|------|------|
| `hm_read_prop0.F` | Read thick-shell property: through-thickness integration points, thickness |

## Description

Thick-shell (also called solid-shell) elements are 8-node hexahedral elements with one element through the thickness, combining shell kinematics with full 3D stress states. The property card defines:
- Number of through-thickness integration points
- Thickness at corner nodes (or uniform thickness)
- Hourglass control parameters

Used for laminated composites where explicit through-thickness stress is needed but a single-element-through-thickness mesh is sufficient (avoids locking vs. full solid).

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `engine/source/elements/thickshell/README.md` — thick-shell element formulations
