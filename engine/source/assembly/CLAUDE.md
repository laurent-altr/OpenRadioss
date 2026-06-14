# engine/source/assembly/

## Purpose
Nodal force and velocity assembly, acceleration computation, and damping.
These routines run inside the `!$OMP PARALLEL` region of `RESOL` each cycle,
after all element force contributions have been computed and MPI-exchanged.

## Key files

| File | Role |
|------|------|
| `accele.F` | Divides total nodal force `NODES%A` by `NODES%MS` → acceleration; applies fixity mask `NODES%IN`; the final step before velocity/coordinate update |
| `accelepinch.F` | `ACCELE` variant for pinched shell nodes |
| `velocity.F` | Leapfrog velocity update: `V(n+1/2) = V(n-1/2) + DT*A(n)` |
| `velocitypinch.F` | Velocity update for pinched nodes |
| `displacement.F` | Coordinate/displacement update: `X(n+1) = X(n) + DT*V(n+1/2)` |
| `displpinch.F` | Displacement for pinched nodes |
| `asspar.F` | Skyline force assembly (Parith/ON, scalar) |
| `asspar3.F` | Skyline force assembly variant 3 |
| `asspar4.F` | Skyline force assembly (Parith/ON, OpenMP-parallel) — called from `RESOL` inside `!$OMP PARALLEL` |
| `asspar5.F` | Skyline force assembly variant 5 |
| `asspar_sub.F`, `asspar_sub_poff.F` | Skyline assembly sub-routines (Parith/ON / OFF) |
| `asspart.F`, `assparxx.F` | Assembly parts and extended assembly |
| `ass2sort.F`, `assadd2.F` | Sorting and incremental assembly helpers |
| `split_asspar4.F` | Split skyline assembly for large models |
| `nlocal_acc.F`, `nlocal_incr.F`, `nlocal_vel.F` | Non-local damage assembly contributions to acceleration/velocity |
| `ply_accele.F`, `ply_velocity.F` | Composite ply contribution to acceleration/velocity |
| `flow_accele.F`, `flow_displ.F`, `flow_velocity.F` | ALE/fluid flow assembly variants |
| `damping.F` | Rayleigh / global damping: applies `-alpha*M*V - beta*K*V` contribution to forces |
| `damping_funct_ini.F90` | Damping function table initialization |
| `damping_vref*.F`, `dampvref_sum6.F` | Velocity-reference damping |
| `disp_vel_saved_cload.F` | Saves displacement/velocity for contact load history |
| `displfakeige.F` | Displacement for fake eigenmode elements |

## Call context in `RESOL`
```
!$OMP PARALLEL
  CALL ASSPAR4(FSKY, FSKYV, FSKYM, …)    ! Parith/ON only
  CALL ACCELE(NODES%A, NODES%AR, NODES%V, NODES%MS, NODES%IN, …)
  ! [leapfrog inline: V += DT*A; X += DT*V]
  CALL DTNODA(…)                          ! or DTNODAMS for AMS
!$OMP END PARALLEL
```

## Dependencies
- Called by: `RESOL` (inside OpenMP parallel region)
- Uses: `NODES` structure (`nodal_arrays_`), `IPARIT` flag
