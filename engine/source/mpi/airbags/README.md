# MPI Airbag Exchanges (`engine/source/mpi/airbags/`)

Cross-domain communication for finite-volume airbag (FVBag) simulations.

## Key Files

| File | Role |
|------|------|
| `spmd_exch_fvstats.F` | Exchange FVBag cell statistics (mass, volume, velocity) across domains |
| `spmd_fvb.F` | Main FVBag MPI exchange dispatcher |
| `spmd_fvb_comm_pattern.F` | Build FVBag inter-domain communication pattern |
| `spmd_fvb_gath.F` | Gather FVBag cell data to root for global pressure update |
| `spmd_fvb_igath.F` | Integer gather for FVBag topology data |

## Description

Finite-volume airbag cells can span MPI domain boundaries. `spmd_fvb_comm_pattern.F` identifies which FVBag faces are on domain boundaries, `spmd_fvb.F` orchestrates the exchange of mass/momentum flux across boundaries each FVBag sub-step, and `spmd_fvb_gath.F` gathers global totals for the ideal-gas pressure update.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/airbag/README.md` — airbag solver
