# LAW83 — Continuum Damage Mechanics (CDM) (`engine/source/materials/mat/mat083/`)

Isotropic continuum damage model: scalar damage variable D (0→1) reduces
effective stiffness; damage grows when equivalent strain exceeds threshold.

## Key Files

| File | Role |
|------|------|
| `sigeps83.F` | Main stress update: damage evolution + stiffness degradation |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
