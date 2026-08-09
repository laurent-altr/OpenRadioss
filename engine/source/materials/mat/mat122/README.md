# LAW122 — Hyperelastic with Mullins Effect (Ogden-Roxburgh) (`engine/source/materials/mat/mat122/`)

Ogden-Roxburgh pseudo-elastic model: captures the Mullins softening effect
in filled rubbers via a damage parameter that reduces strain energy on
unloading.

## Key Files

| File | Role |
|------|------|
| `sigeps122.F` / `sigeps122c.F` | Main solid / shell dispatchers |
| `mat122_newton.F` / `mat122_nice.F` | Solid Newton / NICE solvers |
| `mat122c_newton.F` / `mat122c_nice.F` | Shell Newton / NICE solvers |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
