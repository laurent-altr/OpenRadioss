# Properties Subsystem

This subsystem manages section and property type data for all element types. A **property** (also called a section) defines the geometric and formulation parameters of an element independently of its material law.

In OpenRadioss, the property type is set by `/PROP/TYPE<N>` where `N` identifies the element type family:

| PROP type | Element class | Key parameters |
|-----------|--------------|----------------|
| TYPE1 | Shell (thin) | Thickness, integration points, hourglass control, QEPH |
| TYPE2 | Shell (thick) | Same as TYPE1 + transverse shear |
| TYPE3 | Beam (circular cross-section) | Area, moments of inertia |
| TYPE4 | Beam (general cross-section) | Cross-section table |
| TYPE6 | Spring / dashpot | Stiffness, damping, preload |
| TYPE7 | Solid (reduced integration) | Hourglass control coefficient |
| TYPE8 | Solid (full integration) | Formulation flag |
| TYPE9 | Truss / bar | Cross-section area |
| TYPE10 | SPH particle | Smoothing length, kernel type |
| TYPE11 | Shell offset | Thickness, lamination reference |
| TYPE12 | Thick shell | Thickness, layers |
| TYPE13 | Spring (general) | Multi-DOF stiffness / damping |
| TYPE14 | Solid anisotropic | Material orientation |
| TYPE16 | Joint | Joint type, orientation |
| TYPE17 | Rivet / connector | Connector geometry |
| TYPE18 | Torsion beam | Cross-section, torsion factor |
| TYPE43 | Composite shell | Ply table, reference surface |
| TYPE51 | Composites (ALE) | Composite solid ALE |

## Directory Structure

```
properties/
└── composite_options/
    └── stack/         — Composite laminate stack definitions
```

The main property parsing and data structures are implemented in the starter (`starter/source/properties/`). The engine-side `properties/` directory holds formulation-specific routines for composite element handling.

## Composite Options (`composite_options/`)

For composite shells and solids, additional parameters describe the laminate stack:
- Ply orientations (angles relative to element reference frame)
- Ply thicknesses
- Ply material IDs
- Integration through-thickness scheme (number of points per ply)

The `stack/` subdirectory manages the data structures for ply stacking sequences referenced by TYPE43 and similar.

## Property Interaction with Elements

The element computation routines in `elements/` receive property parameters as input:
- Shell elements: thickness, integration point count, hourglass coefficient
- Solid elements: hourglass control, anisotropy axes
- Beam elements: cross-section geometric data (area, I, J)
- Spring elements: stiffness and damping laws

Property data is stored in the `IPM` (Integer Property Map) and `PM` (Real Property Map) arrays passed to each element group.

## Related Documentation

- `engine/source/elements/README.md` — elements that consume property data
- `engine/source/materials/README.md` — material laws paired with properties
