# Enhanced 8-Node Hexahedral (TYPE14) (`engine/source/elements/solid/solide8/`)

8-node solid with enhanced assumed strain (EAS) or selectively reduced integration — eliminates volumetric locking for nearly-incompressible materials.

## Key Files

| File | Role |
|------|------|
| `s8coor3.F` | Coordinate mapping and Jacobian |
| `s8defo3.F` | Enhanced strain-displacement matrix |
| `s8deri3.F` | Shape function derivatives |
| `s8fint3.F` | Internal force with enhanced strain terms |
| `s8forc3.F` | Force assembly |
| `s8bilan.F` | Force/energy balance |
| `s8cumu3.F` | Cumulative strain tracking |
| `s8eoff.F` | Element offset |

## Formulation

TYPE14 uses EAS (Enhanced Assumed Strains, Simo-Rifai 1990): additional incompatible strain modes are added to eliminate volumetric and shear locking. More accurate than 1-point with hourglass control for bending-dominated and nearly-incompressible problems, at 2–3× higher cost. Commonly used for spot-weld nuggets and adhesive layers.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/solid/solide/README.md` — standard 1-pt hourglass solid
