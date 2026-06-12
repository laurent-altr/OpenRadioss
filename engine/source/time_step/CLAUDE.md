# engine/source/time_step/

## Purpose
Nodal and global time-step computation: determines the critical explicit time step
each cycle from element stiffnesses and nodal masses, then finds the global minimum
across all OpenMP threads and MPI domains.

## Files

| File | Role |
|------|------|
| `dtnoda.F` | Standard nodal time step: `DT_I = SCALE * sqrt(MS_I / STIFN_I)` for each node; takes minimum over thread-owned nodes |
| `dtnodams.F` | AMS (Advanced Mass Scaling) variant: uses AMS effective mass instead of `NODES%MS` |
| `dtnodamp.F` | Adds damping correction to the time step: reduces `DT` when Rayleigh damping is large |
| `dtnodarayl.F` | Rayleigh damping time-step correction (frequency-range damping) |
| `dttherm.F90` | Thermal time step: `DT_THERM = rho*Cp*dx^2 / conductivity` (Fourier stability limit) |
| `find_dt_for_targeted_added_mass.F` | Finds the AMS added mass required to achieve a user-specified target time step |
| `modsti.F` | Modifies stiffness array `NODES%STIFN` for special element conditions |
| `nlocal_dtnoda.F` | Non-local time-step contribution for non-local regularization elements |
| `switch_to_dtnoda.F` | Switches from AMS time-step mode back to standard `DTNODA` |

## Integration in the cycle
Time-step routines are called inside `!$OMP PARALLEL` in `RESOL` (Step 12,
lines ~5780–5960). Each OpenMP thread computes a thread-local minimum `DT2T`;
after the parallel region, the global minimum over threads is taken. The MPI
minimum is implicit in `SPMD_EXCH_A` (which also exchanges `STIFN`/`STIFR`).

```fortran
! Inside !$OMP PARALLEL:
CALL DTNODA(DT2T, NODES%STIFN, NODES%MS, NODES%IN, …)
! After !$OMP END PARALLEL:
DT2 = MIN(DT2T(1:NTHREAD))   ! global min over threads
! MPI min already included via STIFN exchange
```

## Dependencies
- Called by: `RESOL` (inside and after the OpenMP parallel region)
- Uses: `NODES%STIFN`, `NODES%STIFR`, `NODES%MS` (written by element kernels)
