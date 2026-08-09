# Interface Framework (`engine/source/interfaces/interf/`)

Top-level interface framework routines that manage all contact types each time step.

## Key Files

| File | Role |
|------|------|
| `chkstfn3.F` | Check/recompute interface stiffness each step |
| `check_active_elem_edge.F` | Check which element edges are active for edge contact |
| `check_edge_state.F` | Determine edge contact state (active/inactive) |
| `check_nodal_state.F` | Determine nodal contact state per step |
| `check_remote_surface_state.F` | Check contact surface state on remote (ghost) MPI ranks |
| `check_surface_state.F` | Check contact surface state (active/failed elements) |
| `count_nb_elem_edge.F` | Count element edges for edge contact |
| `count_remote_nb_elem_edge.F` | Count remote edges |
| `dealloc_shoot_inter.F` | Deallocate intersection shooting data |
| `find_edge_from_remote_proc.F` | Find contact edges on remote MPI ranks |

## Interface Framework Role

These routines run before the per-type contact loops each time step:
1. `check_surface_state.F` — update segment active/inactive flags (deleted elements no longer contact)
2. `check_edge_state.F` — update edge contact eligibility
3. `chkstfn3.F` — recompute contact stiffness if material properties changed
4. `find_edge_from_remote_proc.F` — exchange ghost segment lists across MPI ranks

## Related Documentation

- `engine/source/interfaces/README.md` — parent interfaces directory
