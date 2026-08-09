# 6-Node Wedge/Penta Solid (`engine/source/elements/solid/solide6z/`)

6-node triangular prism (wedge) solid element used in transition meshes between hex and tet regions.

## Key Files

| File | Role |
|------|------|
| `s6fint_reg.F90` | Internal force with regularised hourglass for wedge |
| `s6zcoor_cp2sp.F90` | Coordinate mapping from corner-to-surface parameterisation |
| `s6zdefc3.F90` | Deformation gradient for wedge |
| `s6zdefo3.F90` | Strain computation |
| `s6zdefot3.F90` | Thermal strain for wedge |

## Formulation

The 6-node wedge element uses triangular cross-section (3 nodes per face) with linear interpolation through the thickness. 2-point Gauss quadrature in the prism direction and 1-point in the triangle face. Used in transition layers between hex-meshed and tet-meshed zones, and for thin-walled prismatic structures. Less accurate than hex elements but necessary for conforming meshes.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
