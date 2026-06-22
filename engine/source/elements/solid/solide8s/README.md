# Stabilised 8-Node Solid (solide8s) (`engine/source/elements/solid/solide8s/`)

Implicit-solver stabilised 8-node hexahedral solid: enhanced formulation with hourglass stabilisation for the implicit solver.

## Key Files

| File | Role |
|------|------|
| `crframe_imp.F` | Co-rotational frame for implicit solver |
| `crtrans_imp.F` | Transformation matrix for implicit |
| `getuloc.F` | Get local element displacements |
| `s8sansb.F` | SANSB (Selectively Averaged Nodal Strain-B) stabilisation |
| `s8sav3_imp.F` | Implicit internal force with SAV stabilisation |

## Formulation

`solide8s` provides stabilisation for the implicit solver to prevent hourglass modes that appear with 1-point quadrature. Uses the SANSB (Selectively Averaged Nodal Strain — B-bar) approach: nodal strain fields are averaged over the element patch to improve conditioning. Used in implicit analyses (forming, springback) where hourglass modes would cause solver convergence issues.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/implicit/README.md` — implicit solver
