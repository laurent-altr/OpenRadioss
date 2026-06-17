# Time Step Subsystem

This subsystem computes the stable critical time step for explicit time integration. The global `dt` is the minimum over all elements, interfaces, and other stability constraints.

## CFL Stability Condition

For explicit time integration, the time step must satisfy the Courant-Friedrichs-Lewy (CFL) condition for each element:

```
dt_elem = scale · L_char / c_sound
```

where `L_char` is the element's characteristic length and `c_sound` is the material wave speed. The global step is:

```
dt = min(dt_elem)   over all elements and constraints
```

The scale factor (default 0.9) is set by `/DT/NODA/CST` or equivalent.

## Key Files

| File | Role |
|------|------|
| `dtnoda.F` | Main nodal time step computation — iterates over all elements |
| `dtnodamp.F` | Nodal DT with mass penalty (Olovsson method) |
| `dtnodams.F` | Nodal DT for AMS (Advanced Mass Scaling) — uses scaled mass |
| `dtnodarayl.F` | Nodal DT with Rayleigh damping correction |
| `dttherm.F90` | Thermal time step condition (Fourier stability) |
| `modsti.F` | Modified stiffness-based DT (for stiff elements) |
| `nlocal_dtnoda.F` | Non-local DT computation (smeared over neighbourhood) |
| `switch_to_dtnoda.F` | Switch from global to nodal DT control |
| `find_dt_for_targeted_added_mass.F` | Compute mass addition needed for a target DT |

## DT Computation Strategies

### Standard Nodal DT (`dtnoda.F`)
Computes the CFL time step element-by-element using the nodal mass and element stiffness:

```
dt_node = sqrt(2·m_node / K_node)
```

This is the default mode activated by `/DT/NODA`.

### Mass-Penalty DT (`dtnodamp.F`)
Adds fictitious mass to under-sized elements to enlarge their stable DT. Unlike AMS, the added mass is global (not mode-selective). Activated by `/DT/NODA/MP`.

### AMS DT (`dtnodams.F`)
When AMS is active (`/DT/AMS`), uses the scaled mass matrix to determine the effective DT, which is the user-specified target step.

### Rayleigh DT (`dtnodarayl.F`)
When Rayleigh damping is active, the damping stiffness contribution modifies the stable step. This file computes the correction.

### Thermal DT (`dttherm.F90`)
For thermal/coupled thermo-mechanical analyses, the Fourier condition limits the thermal step:

```
dt_thermal = scale · ρ·Cp·L² / κ
```

where `κ` is thermal conductivity.

## DT Output

The element with the minimum DT (the "controlling element") is reported in the `.sta` status file and the `.out` output file. This is the most common first thing to check when a run terminates early.

## Related Documentation

- `engine/source/ams/README.md` — Advanced Mass Scaling for enlarged time steps
- `engine/source/assembly/README.md` — uses `dt` for velocity/displacement updates
- `engine/source/output/README.md` — `.sta` file reports current DT and controlling element
