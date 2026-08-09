# LAW80 — Phase-Transformation Thermo-Plastic (Kirkaldy) (`engine/source/materials/mat/mat080/`)

Thermo-elastic-plastic material for steel with solid-state phase transformation:
austenite → martensite/bainite kinetics (Kirkaldy equations), transformation
plasticity, and latent heat.

## Key Files

| File | Role |
|------|------|
| `sigeps80.F` | Main stress update with phase-transformation coupling |
| `sigeps80c.F` | Shell variant |
| `kirkaldykinetics.F` | Kirkaldy isothermal transformation kinetics |
| `phasekinetic2.F` | CCT/TTT phase fraction evolution |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
