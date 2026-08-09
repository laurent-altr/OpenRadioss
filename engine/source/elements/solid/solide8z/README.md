# TYPE24 8-Node Solid (solide8z) (`engine/source/elements/solid/solide8z/`)

8-node hexahedral solid with additional DOFs for enhanced transverse shear and Z-direction strain — used for thick-shell-like solid elements.

## Key Files

| File | Role |
|------|------|
| `get_etfac_s.F` | Get enhanced transverse factor |
| `gethkt3.F` | Hourglass kinematics |
| `gettrans.F` | Get transformation matrix |
| `mmat_h1.F` | Material matrix for H1 enhanced modes |
| `mmats.F` | Material stiffness for solid |

## Formulation

`solide8z` adds Z-direction enhanced strain modes to the standard 8-node hex, enabling accurate prediction of through-thickness compression and interlaminar stresses without using solid-shell (thickshell) elements. Used for bonded joints, sandwich structures, and thick composite plates where interlaminar normal stress is significant.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/thickshell/README.md` — solid-shell alternative
