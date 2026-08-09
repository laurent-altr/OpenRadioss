# 3-Node Shell Initialisation (`starter/source/elements/sh3n/coque3n/`)

Starter initialisation for the standard 3-node Discrete Kirchhoff Triangle (DKT) shell element.

## Key Files

| File | Role |
|------|------|
| `c3init3.F` | Main 3-node shell element initialisation |
| `c3inmas.F` | Compute 3-node shell nodal mass |
| `c3coori.F` | Compute 3-node shell local coordinate system |
| `c3derii.F` | Compute shape function derivatives |
| `c3epsini.F` | Initialise strain state |
| `c3evec3.F` | Compute shell eigenvectors |
| `c3fint_reg_ini.F` | Initialise integration point layout |
| `c3grhead.F` | Write element group header to restart |
| `c3grtails.F` | Write element group tail data to restart |
| `c3veok3.F` | Check element geometry validity |

## Related Documentation

- `starter/source/elements/sh3n/README.md` — parent 3-node shell directory
- `engine/source/elements/sh3n/coque3n/README.md` — engine integration
