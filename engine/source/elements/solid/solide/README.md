# 8-Node Hexahedral Solid (QEPH-Solid) (`engine/source/elements/solid/solide/`)

Main 8-node brick solid element with reduced integration and physical hourglass stabilisation (the default solid in OpenRadioss).

## Key Files

| File | Role |
|------|------|
| `ajac3.F` | Jacobian computation and volume |
| `amass3.F` / `amass3p.F` | Mass matrix (consistent / lumped) |
| `amass3f.F` / `amass3pf.F` | Mass for FVM-coupled solids |
| `amomt3.F` / `amomtn3.F` | Moment of inertia (standard / nodal) |
| `boltst.F` | Bolt preload initialisation for solid elements |
| `check_off_ale.F` | ALE offset check for solid elements |
| `csmall3.F` | Small-strain solid formulation variant |

## Formulation

1-point quadrature at the element centroid with physical hourglass stabilisation for the 8 hourglass modes. The stabilisation uses orthogonal hourglass vectors and a stiffness-based control proportional to the element's material stiffness, avoiding energy-dissipating viscous hourglass. This gives accurate results for nearly-incompressible materials (elastomers, metals at large plastic strain) that would lock with fully-integrated elements.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/solid/solide8/README.md` — TYPE14 enhanced solid
