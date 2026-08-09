# TYPE7 Contact Interface (`engine/source/interfaces/int07/`)

Implements TYPE7 — the most commonly used node-to-segment contact interface in OpenRadioss.

## Key Files

| File | Role |
|------|------|
| `i7mainf.F` | Main TYPE7 contact loop: iterate over contact pairs, call penetration check and force |
| `i7main_lmult.F` | TYPE7 with Lagrange multiplier enforcement (alternative to penalty) |
| `i7for3.F` | Compute contact force: normal penalty + friction tangential |
| `i7cor3.F` | Kinematic correction: remove penetration by velocity correction |
| `i7cor3t.F` | Kinematic correction with time integration |
| `i7cork3.F` | Kinematic correction with stiffness |
| `i7cdcor3.F` | Contact with damping correction |
| `i7ass3.F` | Assemble TYPE7 contact forces to global force vector |
| `i7dst3.F` | Compute distance from node to contact segment |
| `i7dstk3.F` | Distance computation with stiffness |
| `i7ke3.F` | Contact stiffness computation |
| `i7keg3.F` | Global stiffness for TYPE7 |
| `i7lagm.F` | Lagrange multiplier computation for TYPE7 |
| `i7curv.F` | Curved segment contact (non-planar surface) |
| `frictionparts_model.F` | Friction model for TYPE7: Coulomb, viscous, combined |

## TYPE7 Algorithm

TYPE7 is a "node-to-surface" contact:
1. For each slave node: find the closest master segment (broad-phase from `intsort/`)
2. Compute signed gap `g = n̂ · (x_slave - x_seg)`
3. If `g < 0` (penetrating): apply normal force `F_n = -K × g × n̂`
4. Compute tangential relative velocity, apply friction: `F_t = -μ|F_n| × t̂`
5. Assemble forces to slave node and master segment nodes

Stiffness `K` is computed from element properties to be stable (`K = stiffness_scale × E × A / h`).

## Related Documentation

- `engine/source/interfaces/README.md` — parent interfaces directory
- `engine/source/interfaces/intsort/README.md` — broad-phase contact pair sorting
- `engine/source/interfaces/generic/README.md` — voxel search shared by all types
