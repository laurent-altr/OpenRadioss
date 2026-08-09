# 3-Node Shell (`engine/source/elements/sh3n/coque3n/`)

Triangular shell element with standard 3-node formulation (CST-type kinematics with rotational DOFs).

## Key Files

| File | Role |
|------|------|
| `c3coor3.F` | Co-rotational frame and coordinates |
| `c3defo3.F` | Strain computation |
| `c3deri3.F` | Shape function derivatives |
| `c3coef3.F` | Stiffness coefficients |
| `c3bilan.F` | Internal force accumulation |
| `c3be3.F` | Bending energy terms |

## Formulation

The standard 3-node triangular shell. Uses 1-point quadrature at the centroid. Unlike the DKT formulation, this variant uses a simpler strain field with less accurate bending response but lower computational cost. Used in mixed triangle/quad meshes when high bending accuracy is not required (primarily membrane-dominated structures).

## Related Documentation

- `engine/source/elements/sh3n/README.md` — parent sh3n directory
- `engine/source/elements/sh3n/coquedk/README.md` — DKT (higher accuracy)
