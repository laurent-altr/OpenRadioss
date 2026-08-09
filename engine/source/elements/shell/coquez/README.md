# Coquez Shell (`engine/source/elements/shell/coquez/`)

Quadrilateral shell formulation variant with enhanced through-thickness strain (Z-direction) for thick shells and laminated composites.

## Key Files

| File | Role |
|------|------|
| `czbe3.F` | Enhanced Z-strain contribution |
| `czcoork3.F` | Z-direction coordinates |
| `czcorc.F` | Z-direction correction |
| `czcorp5.F` | 5-parameter shell with drilling DOF and Z-strain |

## Formulation

`coquez` extends the standard QEPH kinematics with an enhanced Z-direction strain term. This partially accounts for the transverse (through-thickness) normal strain `ε_33` neglected in standard Kirchhoff-Love / Mindlin-Reissner shells. Used for:
- Thick shells where `t/L ≥ 0.1` (where transverse normal effects are significant)
- Laminated composites with dissimilar layers (avoiding Poisson locking)
- Coupled thermo-mechanical shells where Z-direction thermal expansion matters

## Related Documentation

- `engine/source/elements/shell/README.md` — parent shell directory
- `engine/source/elements/shell/coque/README.md` — standard QEPH without Z-strain
