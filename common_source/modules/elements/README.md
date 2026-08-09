# Element Modules (`common_source/modules/elements/`)

Fortran 90 modules defining element data types shared between starter and engine.

## Modules

| File | Module | Contents |
|------|--------|---------|
| `element_mod.F90` | `ELEMENT_MOD` | Generic element data type: element ID, type code, node connectivity, material/property references |
| `sfem_mod.F90` | `SFEM_MOD` | Spectral Finite Element Method data: NURBS patch descriptors, knot vectors, polynomial degree |

## element_mod

`ELEMENT_MOD` defines a generic element descriptor used at the top level before element-type-specific data is created. It stores:
- Element ID (global)
- Element type code (1=shell, 6=solid, 3=beam, etc.)
- Node ID list (connectivity)
- Property ID, material ID
- Part ID

This is the Fortran 90 typed alternative to legacy `COMMON` blocks for element connectivity.

## sfem_mod

`SFEM_MOD` stores NURBS patch data for isogeometric analysis elements (IGA/IGE3D):
- Knot vectors in each parametric direction (U, V, W)
- Polynomial degree (p, q, r)
- Control point weights
- Patch-to-element mapping

## Related Documentation

- `common_source/modules/README.md` — parent modules directory
- `engine/source/elements/ige3d/README.md` — IGA solid elements using SFEM_MOD
