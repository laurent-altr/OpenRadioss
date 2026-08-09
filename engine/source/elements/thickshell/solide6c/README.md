# Wedge Thick-Shell (solide6c) (`engine/source/elements/thickshell/solide6c/`)

6-node triangular prism solid-shell (thick-shell wedge): TYPE3 thickshell.

## Key Files

| File | Role |
|------|------|
| `s6cbilan.F` | Force/energy balance for wedge thick-shell |
| `s6ccumg3.F` | Cumulative strain |
| `s6cdefo3.F` | Deformation gradient |
| `s6cderi3.F` | Shape function derivatives |
| `s6cdlenmax_sm.F90` | Max edge length for DT estimate |

## Formulation

The wedge thick-shell (triangular prism with one element through thickness) combines the advantages of the triangular shell (easy meshing of complex geometry) with through-thickness integration for stress recovery. Used in transition zones where a hex-based thick-shell mesh transitions to a tet or triangular region. Less accurate than the 8-node variants but necessary for conforming mixed meshes.

## Related Documentation

- `engine/source/elements/thickshell/README.md` — parent thickshell directory
