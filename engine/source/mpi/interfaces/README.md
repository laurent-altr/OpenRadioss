# MPI Interface (Contact) Exchanges (`engine/source/mpi/interfaces/`)

Cross-domain communication for contact search and contact forces in SPMD parallel runs.

## Key Files

| File | Role |
|------|------|
| `intcontp25e.F` | TYPE25 contact candidate exchange |
| `reduce_mmx.F` | Reduce contact min/max penetration across domains |
| `send_cand.F` | Send contact candidate lists to neighbouring domains |
| `sorti25.F` | Sort TYPE25 contact candidates for exchange |
| `spmd_box_limit_reduction.F` | Reduce bounding-box limits for broad-phase contact |
| `spmd_exch_icodt.F` | Exchange contact time step across domains |
| `spmd_exch_icont.F` | Exchange contact interface data |
| `spmd_exch_thknod.F` | Exchange nodal thickness for contact gap computation |
| `spmd_exch_vnpon.F90` | Exchange PON node velocities for contact |
| `spmd_exch_vol.F` | Exchange element volumes for contact stiffness |

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/interfaces/intsort/README.md` — contact search algorithm
