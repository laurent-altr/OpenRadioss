# MPI AMS Exchanges (`engine/source/mpi/ams/`)

Cross-domain communication for Advanced Mass Scaling (AMS/SMS) solver.

## Key Files

| File | Role |
|------|------|
| `spmd_aget_sect.F` | Gather section forces for AMS |
| `spmd_agetmsr.F` | Gather mass-scaling ratios across domains |

## Description

AMS (Advanced Mass Scaling) requires global PCG solver operations that involve all-domain communication beyond the standard ghost-node exchange. `spmd_aget_sect.F` gathers section-force resultants from all domains (needed for AMS constraint satisfaction checks). `spmd_agetmsr.F` gathers the nodal mass-scaling ratios to compute the effective time step globally.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/ams/README.md` — AMS solver
