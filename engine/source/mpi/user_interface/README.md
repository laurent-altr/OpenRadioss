# MPI User Interface Exchange (`engine/source/mpi/user_interface/`)

MPI communication for user-defined window functions (HIC/NIC injury criteria aggregation).

## Key Files

| File | Role |
|------|------|
| `spmd_exch_userwi.F` | Exchange user-window (HIC/NIC) node data across domains |

## Description

`/WINDOW/USER` (HIC, NIC injury criteria) requires aggregating nodal acceleration histories from multiple nodes that may be distributed across MPI domains. `spmd_exch_userwi.F` gathers these nodal time histories to a single domain for the injury criterion computation each step.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `starter/source/tools/userwi/README.md` — user window definition
