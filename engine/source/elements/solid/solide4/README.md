# 4-Node Tetrahedral Solid (`engine/source/elements/solid/solide4/`)

Linear 4-node tetrahedral solid element (constant strain tetrahedron).

## Key Files

| File | Role |
|------|------|
| `a4mass3.F` / `a4mass3p.F` | Mass matrix for tetrahedra |
| `a4momt3.F` / `a4momtn3.F` | Moment of inertia |
| `e4pxle3.F` | Internal force for tet4 element |

## Formulation

The linear tet (tet4) has constant strain throughout the element. It is easy to mesh automatically but is too stiff for bending-dominated problems and exhibits volumetric locking for nearly-incompressible materials (it requires mesh refinement to converge, or use of tet10). In OpenRadioss tet4 is typically used for coarse-mesh region fills in hex-dominant models.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/solid/solide10/README.md` — quadratic tet10 (better accuracy)
