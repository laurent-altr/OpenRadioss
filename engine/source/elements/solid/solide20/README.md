# 20-Node Quadratic Hexahedral Solid (`engine/source/elements/solid/solide20/`)

Serendipity (midside-node) 20-node hexahedral element: high-accuracy solid for smooth stress fields.

## Key Files

| File | Role |
|------|------|
| `s20bilan.F` | Force/energy balance |
| `s20coor3.F` / `s20coork.F` | Coordinates (explicit / co-rotational) |
| `s20cumg3.F` / `s20cumu3.F` | Cumulative strain (global / ULF) |

## Formulation

The 20-node hex uses 2×2×2 Gaussian quadrature (8 integration points) with quadratic shape functions for accurate representation of curved geometry and quadratic stress gradients. It provides much better accuracy than the 8-node hex per DOF for smooth problems (elasticity, vibration) at 4× higher cost. Used for precision simulations (bolt analysis, component fatigue) rather than large-scale crash.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/solid/solide/README.md` — 8-node reduced integration hex
