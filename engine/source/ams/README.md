# Advanced Mass Scaling (AMS / SMS) Subsystem

This subsystem implements **Selective Mass Scaling** (SMS), also known as Advanced Mass Scaling. SMS artificially increases the mass of elements with small stable time steps, allowing a larger global `dt` without stability loss, at the cost of some inertia modification.

Activated by `/DT/AMS` in the input deck.

## How SMS Works

Standard explicit time integration limits `dt` by the smallest element's critical step:

```
dt_crit = L_min / c_sound
```

SMS adds fictitious mass to the **high-frequency modes** of stiff elements only, leaving low-frequency (quasi-static relevant) modes untouched. This is achieved by solving a small constrained mass redistribution problem each time step.

## Key Files

| File | Role |
|------|------|
| `sms_init.F` | SMS initialisation: build mass scaling data structures |
| `sms_build_diag.F` | Build diagonal of the scaled mass matrix |
| `sms_build_mat_2.F` | Assemble the SMS constraint matrix |
| `sms_mass_scale_2.F` | Apply mass scaling (update nodal masses) |
| `sms_pcg.F` | PCG linear solver used inside the SMS iteration |
| `sms_fsa_inv.F` | Forward/backward substitution for SMS |
| `sms_proj.F` | Project velocities to satisfy SMS constraints |
| `sms_encin_2.F` | Compute scaled kinetic energy |
| `sms_bcs.F` | Apply boundary conditions in SMS system |
| `sms_bcs1th.F` | Thermal BC coupling in SMS |
| `sms_bcscyc.F` | Cyclic boundary conditions in SMS |
| `sms_fixvel.F` | Fix prescribed velocity DOFs in SMS |
| `sms_gravit.F` | Gravity body force in SMS context |
| `sms_admesh.F` | Adaptive mesh re-evaluation for SMS |
| `sms_cjoint.F` | Constrained joint handling in SMS |
| `sms_rbe2.F`, `sms_rbe3.F` | Rigid element (RBE2/RBE3) in SMS |
| `sms_rgwal*.F` | Rigid wall constraints in SMS |
| `sms_rlink.F` | Rigid link constraints in SMS |
| `sms_thbcs.F` | Thermal BCS in SMS |

## Processing Sequence

Each explicit time step with SMS active:

```
1. Standard Lagrangian element force computation
2. Standard assembly of forces → nodes
3. SMS iteration:
     a. Compute diagonal mass matrix perturbation (sms_build_diag.F)
     b. Solve SMS constraint system via PCG (sms_pcg.F)
     c. Project velocities (sms_proj.F)
     d. Apply boundary conditions (sms_bcs.F)
4. Continue with velocity/displacement update
```

## Constraints Handled

SMS must account for all kinematic constraints that restrict nodal DOFs:
- Rigid bodies (`sms_rbe2.F`, `sms_rbe3.F`)
- Rigid walls (`sms_rgwal*.F`)
- Rigid links (`sms_rlink.F`)
- Cyclic symmetry (`sms_bcscyc.F`)
- Prescribed BCs (`sms_fixvel.F`, `sms_bcs.F`)

Each constraint type has a dedicated projection routine to ensure the mass-scaled velocities remain consistent with the constraint.

## Linear Solver

The internal SMS linear system is solved by PCG (`sms_pcg.F`), which is a lightweight iterative solver tuned for the SMS system's structure. It is separate from the implicit solver's PCG (`implicit/imp_pcg.F`).

## Time Step Impact

With SMS active, the global time step is controlled by `/DT/AMS` rather than the standard CFL condition. The user specifies a target `dt` and SMS redistributes mass to make it achievable.

## Related Documentation

- `engine/source/assembly/README.md` — nodal mass and assembly
- `engine/source/time_step/README.md` — time step control and AMS DT
- `engine/source/implicit/README.md` — PCG solver (similar principle, different context)
