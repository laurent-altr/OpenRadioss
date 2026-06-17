# Euler 2D (`engine/source/ale/euler2d/`)

2D pure Eulerian solver — elements fixed in space, material flows through them.

## Key Files

| File | Role |
|------|------|
| `eflux2.F` | 2D Euler flux computation across faces |
| `egrad2.F` | 2D gradient reconstruction |
| `edefo2.F` | 2D deformation computation |
| `emomt2.F` | 2D momentum flux |
| `eulro2.F` | 2D Euler density remap |
| `ede112.F` | 2D Euler element type 11 (quadrilateral) force |

## Euler vs ALE

In pure Euler mode the grid is completely fixed — only material flows. This is the limit of ALE when the grid velocity is zero. Used for:
- Free-stream aerodynamics (supersonic flow around blunt bodies)
- High-explosive detonation propagation
- Underwater explosion (water/gas)

The 2D Euler solver handles plane-strain and axisymmetric configurations.

## Related Documentation

- `engine/source/ale/ale2d/README.md` — ALE 2D (grid moves with material)
- `engine/source/ale/README.md` — parent ALE directory
