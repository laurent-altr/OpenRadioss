# ALE Constraints (`starter/source/constraints/ale/`)

Reads ALE-specific boundary conditions: ALE velocity links and ALE boundary condition cards.

## Key Files

| File | Role |
|------|------|
| `hm_read_ale_link_vel.F` | Parse /ALE/LINK/VEL: tie ALE node velocity to Lagrangian node |
| `hm_read_alebcs.F` | Parse /ALEBCS: ALE nodal boundary condition (fixed grid node) |

## Description

`/ALE/LINK/VEL` constrains ALE grid node velocities to track a Lagrangian (structural) node, used at ALE–structure interfaces. `/ALEBCS` fixes individual ALE grid nodes (e.g., hold boundary of the Euler domain fixed while interior nodes rezone).

## Related Documentation

- `starter/source/constraints/README.md` — parent directory
- `starter/source/boundary_conditions/ebcs/README.md` — Euler boundary conditions
- `engine/source/ale/README.md` — ALE engine
