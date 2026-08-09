# DKT Shell Initialisation (`starter/source/elements/sh3n/coquedk/`)

Starter initialisation for the Discrete Kirchhoff Triangle (DKT) 3-node shell formulation.

## Key Files

| File | Role |
|------|------|
| `cdkinit3.F` | Main DKT element initialisation |
| `cdkfint_reg_ini.F` | Initialise DKT integration point layout |
| `cdkderii.F` | Compute DKT shape function derivatives |
| `cdkepsini.F` | Initialise DKT strain state |
| `cdkevec3.F` | Compute DKT eigenvectors |
| `cmaini3.F` | Compute DKT element mass matrix |

## Related Documentation

- `starter/source/elements/sh3n/README.md` — parent directory
- `engine/source/elements/sh3n/coquedk/README.md` — engine DKT integration
