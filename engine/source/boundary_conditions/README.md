# Boundary Conditions Subsystem

This subsystem enforces kinematic and pressure boundary conditions on the model. Two families of BCs are implemented: **BCS** (standard boundary conditions) and **EBCS** (extended boundary conditions for specialised physics).

## Directory Structure

```
boundary_conditions/
├── ebcs/          — Extended boundary conditions (EBCS keywords)
├── bcs_nrf.F90    — Non-reflecting (free-field) boundary condition
└── bcs_wall_trigger.F90  — Wall trigger for BCS activation
```

## Standard Boundary Conditions (BCS)

Standard BCS (`/BCS`) apply constraints on nodal translational and rotational DOFs. These are enforced in the assembly step by zeroing the corresponding force and velocity components. The enforcement is handled centrally in `assembly/` rather than here; this directory handles the EBCS extensions.

## Extended Boundary Conditions (EBCS) — `ebcs/`

EBCS implement physics-based boundary conditions that go beyond simple DOF constraint.

| File | Keyword | Description |
|------|---------|-------------|
| `ebcs4_vel.F` | `/EBCS/VEL` | Prescribed velocity inlet/outlet |
| `ebcs5_normv.F` | `/EBCS/NORMV` | Normal velocity (outflow) BC |
| `ebcs6_inip.F` | `/EBCS/INIP` | Initial pressure BC for fluid domains |
| `ebcs7_iniv.F` | `/EBCS/INIV` | Initial velocity BC for fluid domains |
| `ebcs10_nrf.F` | `/EBCS/NRF` | Non-reflecting (absorbing) boundary — suppresses wave reflections |
| `ebcs11_propellant.F90` | `/EBCS/PROPELLANT` | Solid propellant combustion BC |
| `ebcs12_cyclic.F90` | `/EBCS/CYCLIC` | Cyclic (periodic) symmetry boundary |
| `ebcs123_pres.F` | `/EBCS/PRES` | Pressure boundary for ALE/Euler |
| `ebcclap.F` | — | Clapeyron equation for phase-change BC |
| `ebcs0_gradp0.F` | — | Zero gradient pressure BC |

### Non-Reflecting Boundary (`ebcs10_nrf.F` / `bcs_nrf.F90`)

The NRF (Non-Reflecting / Free-Field) BC prevents artificial wave reflections at artificial truncation boundaries in wave-propagation problems. It applies a dashpot-like force proportional to the outgoing wave speed.

Activated by `/EBCS/NRF` or `/BCS/NRF`.

### Cyclic Symmetry (`ebcs12_cyclic.F90`)

Enforces periodic boundary conditions: nodes on one face of the periodic domain are constrained to mirror the solution on the opposite face. Supports 2D and 3D cyclic sectors.

### Propellant BC (`ebcs11_propellant.F90`)

Models the burning surface of a solid propellant: applies a gas pressure and mass flux based on a burn rate law. Used in rocket motor and detonation simulations.

## BCS Wall Trigger (`bcs_wall_trigger.F90`)

Provides sensor-triggered activation of boundary conditions — a rigid wall can become active only after a specified sensor fires (time, displacement, or force threshold).

## Related Documentation

- `engine/source/loads/README.md` — applied loads (forces, pressures)
- `engine/source/constraints/README.md` — rigid body and kinematic constraints
- `engine/source/ale/README.md` — fluid boundary conditions (ALE/Euler context)
