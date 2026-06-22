# 16-Node Quadratic Thick-Shell (solide16) (`engine/source/elements/thickshell/solide16/`)

16-node serendipity solid-shell with quadratic in-plane shape functions: TYPE4 thickshell.

## Key Files

| File | Role |
|------|------|
| `s16bilan.F` | Force/energy balance |
| `s16deri3.F` | Shape function derivatives (quadratic in-plane) |
| `s16forc3.F` | Internal force assembly |
| `s16rst.F` | Restart I/O for solide16 |
| `s16sigp3.F` | Stress at integration points |

## Formulation

The 16-node thick-shell uses quadratic shape functions in the shell mid-plane (serendipity 8-node quad mapped to 3D) with linear interpolation through the thickness. 3×3 in-plane quadrature with 2–5 through-thickness Gauss points. Provides much higher accuracy than the 8-node variants for smooth stress fields, at 4× higher computational cost. Used for precision springback and component NVH simulations.

## Related Documentation

- `engine/source/elements/thickshell/README.md` — parent thickshell directory
- `engine/source/elements/thickshell/solide8c/README.md` — TYPE2 (lower-order, more common)
