# LAW121 — Hyperelastic (Simo-Pister) (`engine/source/materials/mat/mat121/`)

Compressible hyperelastic material using the Simo-Pister strain-energy
function; suitable for slightly compressible elastomers and soft tissues.

## Key Files

| File | Role |
|------|------|
| `sigeps121.F` / `sigeps121c.F` | Main solid / shell dispatchers |
| `mat121_newton.F` / `mat121_nice.F` | Solid Newton / NICE solvers |
| `mat121c_newton.F` / `mat121c_nice.F` | Shell Newton / NICE solvers |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
