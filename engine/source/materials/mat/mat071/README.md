# LAW71 — Visco-Plastic (Zerilli-Armstrong Modified) (`engine/source/materials/mat/mat071/`)

Physically-based visco-plastic model (modified Zerilli-Armstrong): separate
BCC/FCC formulations with thermally-activated dislocation motion.

## Key Files

| File | Role |
|------|------|
| `sigeps71.F` | Main stress update: ZA rate + temperature model |
| `sigeps71c.F` | Shell variant |
| `sigeps71pi.F` | Plane-stress incompressible variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
