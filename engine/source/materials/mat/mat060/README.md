# LAW60 — Low-Density Foam (Tabulated) (`engine/source/materials/mat/mat060/`)

Highly compressible low-density foam material: separate tabulated loading/
unloading curves, tension cut-off, and strain-rate effects.

## Key Files

| File | Role |
|------|------|
| `sigeps60.F` | Main stress update: nonlinear compaction + unloading |
| `sigeps60c.F` | Shell variant |
| `sigeps60g.F` | SPH variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
