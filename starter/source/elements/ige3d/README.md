# IGA/NURBS Element Initialisation (`starter/source/elements/ige3d/`)

Starter initialisation for 3D isogeometric (IGA) NURBS-based solid elements.

## Key Files

| File | Role |
|------|------|
| `ig3dinit3.F` | Main IGA solid element initialisation |
| `ig3dmass3.F` | Compute IGA element nodal mass |
| `ig3dgrhead.F` | Write IGA element group header to restart |
| `ig3dgrtails.F` | Write IGA element group tail data to restart |
| `lecig3d.F` | Read IGA element connectivity from deck |
| `prelecig3d.F` | Pre-read IGA element data |
| `onebasisfun.F` | Evaluate single NURBS basis function B(ξ) |
| `ig3donebasis.F` | Evaluate all NURBS basis functions at a point |
| `ig3donederiv.F` | Evaluate NURBS basis function derivatives |
| `dersonebasisfun.F` | Compute derivatives of basis functions |
| `comput_coinknot.F` | Compute coincident knot vectors |
| `comput_mesh_neighbour.F` | Build IGA element neighbour connectivity |
| `find_newknot.F` | Locate knot span for a parameter value |
| `meshsurfig3d_mod.F` | IGA surface mesh module |
| `nbadigemesh.F` | Count bad (degenerate) IGA mesh entities |
| `bulkigeo3.F` | Bulk IGA geometry setup |
| `bulkfakeigeo3.F` | Fake (placeholder) IGA geometry for error recovery |
| `rafig3d.F` | IGA mesh refinement (knot insertion) |
| `prerafig3d.F` | Pre-refinement data setup |
| `rebuild_ig3d.F` | Rebuild IGA connectivity after refinement |
| `reorder_ig3d.F` | Reorder IGA control points |
| `searchigeo3d.F` | Search IGA element for given parametric coordinates |
| `test_support_fct.F` | Test NURBS support function validity |
| `test_support_newfct.F` | Test new NURBS support functions |

## Description

IGA replaces Lagrange polynomial basis functions with NURBS (Non-Uniform Rational B-Splines) for exact geometric representation of curved boundaries. `ig3dinit3.F` sets up the NURBS knot vectors, control point arrays, and integration point layout. `rafig3d.F` performs knot insertion to refine the IGA mesh without changing the geometry.

## Related Documentation

- `starter/source/elements/README.md` — parent directory
- `engine/source/elements/solid/README.md` — engine solid integration
