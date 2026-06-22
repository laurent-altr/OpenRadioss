# LAW86 — Visco-Elastic Maxwell (`engine/source/materials/mat/mat086/`)

Generalised Maxwell visco-elastic material: elastic spring in series with
N dashpot-spring (Maxwell) elements; long-term modulus G∞ plus Prony series.

## Key Files

| File | Role |
|------|------|
| `sigeps86c.F` | Shell stress-update with Maxwell chain integration |
| `sigeps86g.F` | SPH variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/materials/visc/README.md` — shared Prony utilities
