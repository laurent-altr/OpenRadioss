# LAW190 — Tabulated Composite Progressive Damage (`engine/source/materials/mat/mat190/`)

Tabulated orthotropic composite damage: direction-dependent stiffness
degradation driven by tabulated damage-equivalent-strain curves; supports
both solid and shell elements.

## Key Files

| File | Role |
|------|------|
| `sigeps190.F` | Main stress update: tabulated damage lookup + stiffness degradation |
| `condamage.F` | Damage-consistency condition and damage-variable update |
| `conversion.F` | Material-frame coordinate conversion |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
