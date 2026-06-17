# MPI Element Exchanges (`engine/source/mpi/elements/`)

Ghost-node and inter-domain exchanges for element data: deformed geometry, thickness, stiffness, and element-activation tags.

## Key Files

| File | Role |
|------|------|
| `spmd_exch_a_scnd.F` | Exchange secondary node acceleration (ghost node update) |
| `spmd_exch_a_scnd_pon.F` | Secondary acceleration for PON (pinned-on) nodes |
| `spmd_exch_dttsh.F` | Exchange thick-shell element DT across domains |
| `spmd_exch_stif_scnd.F` | Exchange secondary stiffness for implicit solver |
| `spmd_exch_tag_scnd.F` | Exchange element activation tags across domains |

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/mpi/nodes/README.md` — nodal state exchange
