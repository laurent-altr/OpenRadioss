# Euler 3D (`engine/source/ale/euler3d/`)

3D pure Eulerian solver — fixed-grid fluid computation.

## Key Files

| File | Role |
|------|------|
| `eflux3.F` | 3D Euler flux computation across hexahedral faces |
| `egrad3.F` | 3D gradient reconstruction |

## Purpose

3D Euler solver for high-speed compressible flow where the mesh is fixed:
- Blast wave propagation in air (HE detonation far-field)
- Hypervelocity impact (projectile into target, grid fixed to target)
- Underwater shock (water mesh fixed, explosive charge Lagrangian)

The Euler grid is typically coarser than the Lagrangian structure mesh. Coupling between the fixed Euler grid and the moving structure is handled in `engine/source/ale/inter/`.

## Related Documentation

- `engine/source/ale/euler2d/README.md` — 2D Euler
- `engine/source/ale/README.md` — parent ALE directory
