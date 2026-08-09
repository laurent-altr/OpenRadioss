# MPI Seatbelt Exchanges (`engine/source/mpi/seatbelts/`)

Cross-domain communication for seatbelt (1D cable/retractor) elements spanning domain boundaries.

## Key Files

| File | Role |
|------|------|
| `spmd_exch_a_seatbelt.F` | Exchange seatbelt node accelerations across domain boundaries |

## Description

Seatbelt elements are 1D elements (cables, retractors, slip rings) that can cross domain boundaries in long-vehicle models. `spmd_exch_a_seatbelt.F` exchanges the acceleration of shared seatbelt nodes between the owning domain and the domain containing the belt element, ensuring consistent force application along the full belt path.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `starter/source/tools/seatbelts/README.md` — seatbelt definition
