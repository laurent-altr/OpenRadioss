# Starter Stack Subsystem

This subsystem reads and initialises composite laminate ply stacking sequence definitions (`/STACK`).

## Key Files

| File | Role |
|------|------|
| `stackgroup.F` | Read `/STACK` keyword — ply sequence definition |
| `stackgroup_drape.F` | `/STACK` with drape mapping (orientation from draping simulation) |
| `pres_stackgroup.F` | Pressure ply stack group handling |
| `hm_read_stack.F` | HyperMesh-format stack reader |

## Stack Definitions

A `/STACK` keyword defines a composite laminate by listing plies in sequence:

```
/STACK/<ID>
Ply1: material_id, orientation_angle, thickness
Ply2: material_id, orientation_angle, thickness
...
```

Each ply references a material law (typically an orthotropic composite law like LAW25, LAW58, or LAW131) and specifies its thickness and fiber angle relative to the element reference frame.

## Drape Mapping (`stackgroup_drape.F`)

For formed composite parts, the fiber orientation varies across the surface due to the forming process. `/STACK/DRAPE` reads orientation angles from an external draping simulation result file and maps them to the FE mesh.

## Integration with Properties

Stack definitions are referenced by shell properties (`/PROP/TYPE43`, `/PROP/TYPE51`). The property links the element to its ply stack, which determines the through-thickness integration scheme (one integration point per ply for explicit, multiple for accuracy).

## Related Documentation

- `starter/source/properties/README.md` — composite property types (TYPE43) that reference stacks
- `engine/source/elements/README.md` — shell element integration through composite plies
