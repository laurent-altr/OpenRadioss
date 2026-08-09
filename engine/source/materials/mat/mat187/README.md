# LAW187 — Phase-Field Fracture (`engine/source/materials/mat/mat187/`)

Phase-field model for brittle and ductile fracture: damage field φ evolves
via a Ginzburg-Landau equation; crack is represented as a diffuse band.
Couples elasticity with a phase-field degradation function g(φ).

## Key Files

| File | Role |
|------|------|
| `sigeps187.F` | Main stress update: elastic predictor with phase-field degradation |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
