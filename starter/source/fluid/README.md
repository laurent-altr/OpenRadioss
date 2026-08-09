# Starter Fluid Subsystem

This subsystem reads and initialises the incompressible fluid and DAA (acoustic BEM) interface definitions.

## Key Files

| File | Role |
|------|------|
| `flowdec.F` | Flow element declaration and mesh identification |
| `init_qd.F` | Initialise quadrilateral fluid mesh (QD4 elements) |
| `init_tg.F` | Initialise triangular fluid mesh (TG3 elements) |
| `intanl_qd.F` | Analytical integral for QD4 BEM formulation |
| `intanl_tg.F` | Analytical integral for TG3 BEM formulation |

## BEM Initialisation

For acoustic fluid-structure interaction (underwater shock, noise), the boundary element method requires a pre-computation of influence coefficients:
- `init_qd.F` / `init_tg.F` identify wet surface elements (fluid-structure interface)
- `intanl_qd.F` / `intanl_tg.F` compute the BEM influence matrix entries analytically

These influence matrices are stored in the restart file for the engine's BEM solver (`engine/source/fluid/bemsolv.F`).

## Related Documentation

- `engine/source/fluid/README.md` — runtime BEM/incompressible fluid solver
