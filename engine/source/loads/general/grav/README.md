# Gravity Body Force (`engine/source/loads/general/grav/`)

Applies gravitational and other body-force acceleration fields to all nodes each time step.

## Key Files

| File | Role |
|------|------|
| `gravit.F` | Apply gravity body force: `F = m × g_vec` for each node |
| `gravit_fvm_fem.F` | Gravity for hybrid FVM/FEM (ALE finite-volume + FEM coupling) |
| `gravit_imp.F` | Gravity contribution to implicit solver residual and stiffness |

## Algorithm

Gravity is a body force applied uniformly in the user-specified direction vector `(gx, gy, gz)`. Each step:

```fortran
F_node(1:3) += mass_node * g_vec(1:3) * fscale(t)
```

`fscale(t)` allows ramped gravity (via `/FUNCT`) for quasi-static initialisation. `gravit_fvm_fem.F` handles the additional volume-averaged body force term needed when ALE finite-volume cells overlap FEM nodes.

## Related Documentation

- `engine/source/loads/general/README.md` — parent directory
- `engine/source/loads/README.md` — all load types
