# TYPE22 Interface (`engine/source/interfaces/int22/`)

TYPE22 — Immersed boundary ALE contact (Lagrangian immersed in ALE Euler grid).

## Key Files

| File | Role |
|------|------|
| `deltax22.F` | Compute displacement increment at TYPE22 interface |
| `destroy_cell.F` | Destroy ALE cells intersected by Lagrangian structure |
| `get_group_id.F` | Get group ID for ALE interface elements |

## TYPE22

TYPE22 handles the case where a Lagrangian structure is immersed in a fixed Eulerian grid. As the structure moves:
1. The structure cuts through Euler cells
2. Cut cells are partially or fully occupied by the structure
3. The ALE solver adjusts fluxes at cut cells to enforce the structural boundary condition

`destroy_cell.F` marks Euler cells that become fully inside the structure as void. This is the "volume-of-fluid" approach to immersed boundary tracking.

## Related Documentation

- `engine/source/interfaces/README.md` — parent directory
- `engine/source/ale/inter/README.md` — ALE-Lagrangian coupling
