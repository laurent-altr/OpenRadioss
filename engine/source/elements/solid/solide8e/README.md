# EAS-Enhanced 8-Node Solid (solide8e) (`engine/source/elements/solid/solide8e/`)

8-node hexahedral solid with enhanced assumed strains for the implicit solver.

## Key Files

| File | Role |
|------|------|
| `jacob_f0.F` | Jacobian at reference configuration |
| `jacob_j33.F` | Jacobian `J33` component for EAS |
| `m1tot_stab11.F` / `m1tot_stab18.F` | Stabilisation for EAS variants |
| `nsvis_stab18.F` | Nodal viscosity for 18-parameter EAS |

## Formulation

`solide8e` provides the EAS (Enhanced Assumed Strain) formulations with 11 or 18 enhanced parameters for use in the implicit solver. The enhanced parameters are condensed at the element level (static condensation) before assembly into the global stiffness matrix. With 18 enhanced parameters, full locking-free behaviour is achieved for bending and nearly-incompressible states. Higher-parameter variants are used for accurate springback simulation.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/solid/solide8/README.md` — explicit EAS variant
