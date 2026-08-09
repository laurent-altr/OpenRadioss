# MPI ALE Exchanges (`engine/source/mpi/ale/`)

Cross-domain communication for ALE/Euler solver quantities: fluid quantities at domain interfaces, ALE rezoning neighbour checks.

## Key Files

| File | Role |
|------|------|
| `spmd_exch_min_max.F90` | Exchange min/max pressure/velocity across ALE domains |
| `spmd_exch_n_neighbor.F90` | Exchange neighbour cell lists for ALE rezoning |
| `spmd_check_tag.F` | Check ALE cell tags across domain boundaries |
| `spmd_exch_nodnx.F` | Exchange nodal ALE velocity (Nx direction) |
| `spmd_exch_sms.F` | Exchange SMS (selective mass scaling) ALE data |
| `spmd_exch_sms6.F` | Exchange 6-DOF SMS ALE data |
| `spmd_exch_smst2.F` | Exchange SMS TYPE2 ALE data |

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/ale/README.md` — ALE solver
