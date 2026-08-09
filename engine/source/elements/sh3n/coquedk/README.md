# DKT Triangle Shell (`engine/source/elements/sh3n/coquedk/`)

Discrete Kirchhoff Triangle (DKT) — a high-accuracy thin-shell triangular element.

## Key Files

| File | Role |
|------|------|
| `cdkcoor3.F` | Co-rotational coordinates |
| `cdkdefo3.F` | DKT strain computation (bending modes from DKT interpolation) |
| `cdkderi3.F` | DKT shape function derivatives |
| `cdkfint3.F` | Internal force assembly |
| `cdkfcum3.F` | Cumulative force accumulation |

## Formulation

DKT (Discrete Kirchhoff Theory) imposes the Kirchhoff constraint (zero transverse shear) at discrete points around the element boundary. This gives excellent bending accuracy for thin shells with no shear locking, at the cost of more complex shape functions. DKT triangles are the preferred triangular shell for thin structures (airframes, sheet metal).

## Related Documentation

- `engine/source/elements/sh3n/README.md` — parent sh3n directory
- `engine/source/elements/sh3n/coque3n/README.md` — simpler 3-node shell
- `engine/source/elements/sh3n/coquedk6/README.md` — DKT6 (6-node quadratic)
