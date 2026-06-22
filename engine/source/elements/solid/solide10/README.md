# 10-Node Quadratic Tetrahedral Solid (`engine/source/elements/solid/solide10/`)

Quadratic 10-node tetrahedral element with midside nodes — much better accuracy than tet4 for curved geometry and bending.

## Key Files

| File | Role |
|------|------|
| `s10bilan.F` | Force/energy balance for tet10 |
| `s10cndf.F` | Condensed internal forces |
| `s10sigp3.F` | Stress at integration points |
| `s10volj.F` | Volume/Jacobian |
| `s10volnod3.F` / `s10volnodt3.F` | Volume per node (standard / thermal) |
| `s10_icp.F` | Integration point coordinates |
| `m1tot_stab_p.F` | Stabilisation for tet10 |
| `nsvis_sm12.F` / `nsvis_stab.F` | Nodal viscosity stabilisation |

## Formulation

Tet10 uses 4-point quadrature (one per sub-tetrahedron) and quadratic shape functions. It passes the patch test and provides second-order accuracy for smooth stress fields. Preferred over tet4 for crash simulations using auto-tet meshed components (B-pillars, brackets, castings). The `nsvis_stab.F` applies a nodal viscosity stabilisation to suppress high-frequency noise from the quadratic shape functions.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/solid/solide4/README.md` — linear tet4
