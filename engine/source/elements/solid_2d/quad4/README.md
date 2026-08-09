# Enhanced 2D Quad4 Solid (`engine/source/elements/solid_2d/quad4/`)

Enhanced 4-node quadrilateral 2D solid element with EAS (Enhanced Assumed Strains) for improved accuracy.

## Key Files

| File | Role |
|------|------|
| `q4coor2.F` | Coordinates and Jacobian |
| `q4defo2.F` / `q4defoc2.F` | Standard / EAS-enhanced strain computation |
| `q4deri2.F` / `q4deric2.F` | Standard / EAS shape function derivatives |
| `q4fint2.F` | Internal force with EAS |
| `q4cumu2.F` / `q4cumu2p.F` | Cumulative strain (standard / plane stress) |

## Formulation

`quad4` adds EAS incompatible modes to the standard 2D quad, eliminating volumetric locking for nearly-incompressible materials (rubber, plasticity at large strain) and improving accuracy for bending-dominated 2D problems. Used for 2D cross-section analyses of tyres, seals, and pressure vessels where the locking-free behaviour is essential for correct strain localisation.

## Related Documentation

- `engine/source/elements/solid_2d/README.md` — parent 2D solid directory
- `engine/source/elements/solid_2d/quad/README.md` — basic 2D quad (1-point)
