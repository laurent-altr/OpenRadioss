# Engine Composite Options (`engine/source/properties/composite_options/`)

Engine-side composite property management: ply stack data access and composite stiffness evaluation.

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `stack/` | Access to ply stack data at engine runtime |

## Description

This directory mirrors the structure of `starter/source/properties/composite_options/` at the engine level. At engine startup, the ply stack data (ply angles, thicknesses, material IDs) read by the starter is loaded into the ply parameter arrays. The engine then uses this data each step for:
- Setting the fiber angle for each through-thickness integration point
- Computing the laminated stiffness matrix for composite shell elements
- Driving ply-by-ply failure criteria

## Related Documentation

- `starter/source/properties/composite_options/README.md` — ply stack definition in starter
- `common_source/modules/mat_elem/README.md` — ply_param_mod data structure
