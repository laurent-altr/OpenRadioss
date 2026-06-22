# Tet10 Solid Initialisation (`starter/source/elements/solid/solide10/`)

Starter initialisation for 10-node quadratic tetrahedral solid elements.

## Key Files

| File | Role |
|------|------|
| `s10init3.F` | Main tet10 element initialisation |
| `s10mass3.F` | Compute tet10 nodal mass |
| `s10coor3.F` | Compute tet10 coordinate system |
| `s10deri3.F` | Compute tet10 shape function derivatives |
| `s10jaci3.F` | Compute tet10 Jacobian |
| `s10len3.F` | Compute tet10 characteristic length |
| `dim_s10edg.F` | Compute dimension of tet10 edge arrays |
| `s10edg_rlink.F` | Set up tet10 edge-node rigid links (mid-side nodes) |

## Related Documentation

- `starter/source/elements/solid/README.md` — parent directory
- `engine/source/elements/solid/solide10/README.md` — engine tet10 integration
