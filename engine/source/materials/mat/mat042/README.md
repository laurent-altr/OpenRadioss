# LAW42 — Composite Progressive Damage (Ladeveze) (`engine/source/materials/mat/mat042/`)

Mesomodel for UD composite plies (Ladeveze 1992): coupled fiber/matrix
damage mechanics with elastic-plastic matrix shear. Shell elements only.

## Key Files

| File | Role |
|------|------|
| `sigeps42.F` | Main stress update: damage-driven stiffness degradation |
| `sigeps42c.F` | Shell (coque) variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
