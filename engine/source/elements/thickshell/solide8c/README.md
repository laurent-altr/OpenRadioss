# Enhanced Thick-Shell (solide8c) (`engine/source/elements/thickshell/solide8c/`)

Enhanced 8-node solid-shell with improved transverse shear and hourglass control: TYPE2 thickshell.

## Key Files

| File | Role |
|------|------|
| `s8cdefo3.F` | Enhanced deformation gradient |
| `s8cfint_reg.F` | Regularised internal force with improved hourglass |
| `s8cforc3.F` | Force assembly |
| `s8cke3.F` | Element stiffness for implicit |
| `s8csigp3.F` | Stress at integration points |

## Formulation

TYPE2 adds assumed natural strain (ANS) treatment for transverse shear locking and EAS modes for volumetric locking, on top of the basic solidec kinematics. This eliminates the transverse shear locking that occurs in thin-limit behaviour and gives significantly more accurate stress recovery through the thickness. Recommended over solidec for most thick-shell applications.

## Related Documentation

- `engine/source/elements/thickshell/README.md` — parent thickshell directory
- `engine/source/elements/thickshell/solidec/README.md` — basic thick-shell
