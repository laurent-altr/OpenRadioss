# Assembly Subsystem

This subsystem performs the gather/scatter operations that map between element-local quantities and global nodal arrays — the key step between element force computation and the time integration update.

## Role in the Time Loop

```
elements/ → internal forces (element-local)
                    │
                    ▼
           assembly/asspar*.F   ← scatter to global nodal force array
                    │
                    ▼
           assembly/accele.F    ← compute nodal acceleration  (F/m)
                    │
                    ▼
           assembly/velocity.F  ← update nodal velocity  (v += a·dt)
                    │
                    ▼
           assembly/displacement.F  ← update nodal position  (x += v·dt)
```

## Key Files

| File | Role |
|------|------|
| `asspar.F` | Main parallel scatter: element forces → nodal force vector |
| `asspar3.F`, `asspar4.F`, `asspar5.F` | Variants for different element/partition layouts |
| `asspar_sub.F`, `asspar_sub_poff.F` | Sub-partitioned assembly helpers |
| `asspart.F` | Partition-level assembly |
| `assparxx.F` | Extended assembly for special cases |
| `ass2sort.F`, `assadd2.F` | Sorting and addition helpers |
| `accele.F` | Compute nodal acceleration from net force and nodal mass |
| `accelepinch.F` | Acceleration for pinch/edge nodes |
| `velocity.F` | Explicit velocity update (`v_n+½ = v_n-½ + a·dt`) |
| `velocitypinch.F` | Velocity update for pinch nodes |
| `displacement.F` | Position update (`x_n+1 = x_n + v_n+½·dt`) |
| `displpinch.F` | Displacement update for pinch nodes |
| `displfakeige.F` | Displacement for IGE3D virtual nodes |
| `damping.F` | Global Rayleigh / viscous damping force |
| `damping_funct_ini.F90` | Damping function initialisation |
| `damping_vref.F` | Velocity-reference damping |
| `damping_vref_compute_dampa.F90` | Compute damping coefficient |
| `damping_vref_rby.F90`, `damping_vref_rby_stiff.F90` | Rigid body damping |
| `damping_vref_sum6_rby.F90`, `dampvref_sum6.F` | 6-DOF damping summation |
| `flow_accele.F`, `flow_velocity.F`, `flow_displ.F` | ALE flow field kinematics |
| `ply_accele.F`, `ply_velocity.F` | Ply-based shell kinematics |
| `nlocal_acc.F`, `nlocal_vel.F`, `nlocal_incr.F` | Non-local (averaged) kinematics |
| `split_asspar4.F` | Split-domain assembly for SPMD |
| `disp_vel_saved_cload.F` | Save/restore for contact load application |

## Parallelism

- **OpenMP**: The scatter operations in `asspar*.F` are designed to avoid race conditions by sorting elements into non-conflicting colour groups (elements sharing no nodes are the same colour). Each colour is assembled in parallel with OpenMP.
- **MPI**: Forces from ghost elements (elements shared across MPI domain boundaries) are reduced using `SPMD_ALLREDUCE` after local scatter. See `mpi/README.md`.

## Damping

Rayleigh damping adds a force proportional to velocity:

```
F_damp = -α·M·v - β·K·v
```

where `α` (mass-proportional) and `β` (stiffness-proportional) are user-defined. The `damping_vref*.F90` files implement a reference-velocity variant used in crash simulations.

## ALE Kinematics

`flow_accele.F`, `flow_velocity.F`, and `flow_displ.F` handle the mesh velocity degree of freedom in ALE elements, which is separate from the material velocity.

## Related Documentation

- `engine/source/elements/README.md` — produces the element-local forces
- `engine/source/mpi/README.md` — inter-domain force reduction
- `engine/source/time_step/README.md` — determines `dt` used in velocity/displacement updates
