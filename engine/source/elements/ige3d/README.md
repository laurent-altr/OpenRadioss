# Isogeometric 3D Elements (`engine/source/elements/ige3d/`)

Computes internal forces for isogeometric analysis (IGA) solid elements. IGA elements use NURBS (Non-Uniform Rational B-Splines) as basis functions instead of Lagrange polynomials, giving smooth stress fields and exact geometry representation.

## Key Files

| File | Role |
|------|------|
| `ig3dfint.F` | Main internal force computation for IGA solids |
| `ig3ddefo.F` | Compute deformation gradient `F` in NURBS parameter space |
| `ig3dcumu3.F` | Accumulate IGA element forces into global arrays |
| `ig3daire.F` | Compute element volume (area × thickness for shells) in parameter space |
| `ig3daverage.F` | Average stress/strain over the NURBS element |
| `ig3dderishap.F` | Compute NURBS basis function derivatives (B-spline) |
| `ig3donebasis.F` | Evaluate one B-spline basis function at a parametric point |
| `ig3donederiv.F` | Evaluate derivative of one basis function |
| `ig3duforc3.F` | Scatter IGA element forces to nodal force vector |
| `ige3ddefo.F` | Deformation gradient computation (alternative path) |
| `ige3dbilan.F` | IGA element energy balance |
| `ige3dzero.F` | Zero element-level working arrays |
| `onebasisfun.F` | B-spline single basis function evaluation (used by ig3d*) |
| `dersbasisfuns.F` | Derivatives of all B-spline basis functions in a knot span |
| `dersonebasisfun.F` | Derivatives of one B-spline basis function |
| `projecig3d.F` | Project IGA result quantities onto Lagrange nodes for output |

## IGA in OpenRadioss

Unlike classical FE, IGA elements:
- Share basis functions (NURBS) with the CAD geometry — no geometry approximation error
- Use `k`-refinement (increase degree and smoothness simultaneously)
- Have inter-element continuity up to C^(p-1) where p is the NURBS degree
- Are more expensive per DOF but converge faster per DOF for smooth problems

The element is defined by a NURBS patch with control points, knot vectors, and polynomial degree. Multiple elements share the same NURBS patch and are distinguished by their parametric domain (knot spans).

## Related Documentation

- `engine/source/elements/README.md` — parent elements directory overview
- `starter/source/elements/ige3d/` — IGA element initialisation
