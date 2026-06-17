# Thick-Shell (solidec) (`engine/source/elements/thickshell/solidec/`)

8-node hexahedral solid-shell (thick-shell) element with standard integration through the thickness: TYPE1 thickshell.

## Key Files

| File | Role |
|------|------|
| `scdefc3.F` | Deformation gradient for thick-shell |
| `accdtdc.F` | Time step estimate for thick-shell element |
| `dim_tshedg.F` | Dimension of thick-shell edge data |
| `ind_tshedg.F` | Index arrays for thick-shell edge connectivity |
| `sc8dlenmax_sm.F90` | Maximum edge length for DT estimate |

## Formulation

`solidec` is the standard thick-shell: 8-node hex with shell-like kinematics (one element through thickness, linear transverse normal strain). Uses reduced in-plane integration (1-point) with hourglass stabilisation and full through-thickness integration (2–5 Gauss points). Suitable for moderately thick plates and sandwich structures where in-plane membrane/bending and through-thickness stress are both important.

## Related Documentation

- `engine/source/elements/thickshell/README.md` — parent thickshell directory
- `engine/source/elements/thickshell/solide8c/README.md` — enhanced variant
