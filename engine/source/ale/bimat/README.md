# Bi-Material ALE (`engine/source/ale/bimat/`)

Implements the bi-material (two-fluid) interface tracking in ALE — when two different materials share the same ALE mesh.

## Key Files

| File | Role |
|------|------|
| `amulf2.F` | 2D bi-material flux: compute fluxes for mixed cells |
| `bafil2.F` | 2D bi-material fill fraction update |
| `balph2.F` | 2D material interface angle computation |
| `bamom2.F` | 2D bi-material momentum |
| `bcumu2.F` | 2D bi-material force accumulation |
| `befil2.F` | 2D energy fill update |
| `bemom2.F` | 2D energy-momentum |
| `bforc2.F` | 2D bi-material force |
| `blero2.F` | 2D bi-material remap |
| `bmultn.F` | Multi-material normal computation |

## Bi-Material Model

When two materials coexist in the same ALE cell (e.g., air and water after a dam break), the cell is "mixed". The bi-material model:
1. Tracks the volume fraction `α` of each material per cell
2. Uses a Young-type (piecewise-linear) interface reconstruction to locate the material boundary within the cell
3. Computes fluxes for each material separately
4. Ensures pressure equilibrium between coexisting materials

This is a simpler alternative to the full multi-material (`multifluid`) model — limited to exactly two materials.

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `engine/source/multifluid/README.md` — multi-material (>2 materials) extension
