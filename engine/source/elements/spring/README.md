# Spring/Damper Force Computation (`engine/source/elements/spring/`)

Computes forces for spring and damper elements each time step.

## Key Files

| File | Role |
|------|------|
| `r1coor3.F` | Compute spring orientation (current axis vector) |
| `r1coork3.F` | Spring kinematics in local frame |
| `r1def3.F` | Compute spring deformation (extension, rotation) |
| `r1len3.F` | Compute current spring length |
| `r1cum3.F` / `r1cum3p.F` | Accumulate spring forces to global array (serial / parallel) |
| `r1sens3.F` | Evaluate spring sensor (for triggered deactivation) |
| `r1tors.F` | Torsional spring force computation |
| `r12ke3.F` / `r12mat3.F` / `r12sumg3.F` | TYPE12 spring material and force |
| `r13ke3.F` / `r13mat3.F` / `r13sumg3.F` | TYPE13 pulley spring |
| `preload_axial.F90` | Compute axial preload force for pretensioned springs |

## Spring Force Algorithm

For each spring element:
1. Compute current length/angle from nodal positions (`r1len3.F`)
2. Compute deformation relative to natural length (`r1def3.F`)
3. Evaluate force-displacement law (linear or from `/FUNCT` curve)
4. Add damping: `F_damp = C × dL/dt`
5. Scatter force along spring axis to both nodes (`r1cum3.F`)

TYPE12 springs (`r12*`) support coupling between multiple DOF (axial + shear + torsion), failure, and user material laws.

## Related Documentation

- `starter/source/elements/spring/README.md` — spring initialisation
- `engine/source/elements/README.md` — parent elements overview
