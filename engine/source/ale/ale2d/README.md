# ALE 2D (`engine/source/ale/ale2d/`)

2D ALE (Arbitrary Lagrangian-Eulerian) advection step for quad elements.

## Key Files

| File | Role |
|------|------|
| `aconv2.F` | 2D ALE convection: advect density, momentum, energy across element faces |
| `adiff2.F` | 2D ALE diffusion: smooth transported quantities |
| `afimp2.F` | 2D ALE implicit mass flux computation |
| `aflux2.F` | 2D ALE explicit mass flux across faces |
| `agrad2.F` | 2D gradient computation for ALE transport (upwind reconstruction) |
| `amomt2.F` | 2D ALE momentum advection |
| `alero2.F` | 2D ALE density/energy remapping |
| `arezo2.F` | 2D ALE rezoning (grid velocity computation) |
| `bconv2.F` | 2D advection — B-type (alternative scheme) |
| `bhol2.F` | 2D ALE hole correction (for voids) |
| `brezo2.F` | 2D rezoning B-type |
| `iqel02.F` | 2D element quality metric |
| `tgrad2.F` | 2D thermal gradient advection |

## ALE Operator Splitting

The 2D ALE step follows Lagrangian → Rezone → Advect:
1. **Lagrangian step** (in `engine/source/elements/solid_2d/`): compute forces, update velocities, move nodes
2. **Rezone** (`arezo2.F`): compute grid velocity to move mesh toward reference config
3. **Advect** (`aconv2.F`, `aflux2.F`): transport conserved quantities (ρ, ρu, ρE) across the moving-mesh faces using van Leer or MUSCL

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `engine/source/ale/alemuscl/README.md` — MUSCL advection scheme
