# Engine Tools Subsystem

This subsystem provides general-purpose utility routines used across the engine — function interpolation, sensor evaluation, cross-section forces, coordinate frame transforms, and other helpers that do not belong to a single physics subsystem.

## Directory Structure

```
tools/
├── accele/        — Accelerometer utility (sensor-based acceleration output)
├── curve/         — Load curve / function interpolation
├── lagmul/        — Lagrange multiplier utilities
├── seatbelts/     — Seatbelt and guided cable element utilities
├── sect/          — Cross-section (section cut) force computation
├── sensor/        — Sensor evaluation (force, displacement, energy, velocity sensors)
├── skew/          — Skew frame / local coordinate system transforms
├── univ/          — Universal joint kinematics
└── finter_mixed.F90 — Mixed-mode function interpolation
```

## Function Interpolation (`curve/`)

| File | Role |
|------|------|
| `finter.F` | Main function interpolation (linear, piecewise) |
| `finter_smooth.F` | Smoothed function interpolation (for noise reduction) |
| `lecfun.F` | Read function (`/FUNCT`) definition from restart |
| `interp.F` | General 1D interpolation primitives |
| `funct_python_update_elements.F90` | Update function values from Python co-simulation |

Every time-varying load, boundary condition, or output trigger references a `/FUNCT` function curve. `finter.F` is the core look-up used throughout the engine.

## Sensors (`sensor/`)

Sensors monitor physical quantities and trigger events (load activation, termination, output).

| File | Role |
|------|------|
| `rsens_nic.F` | Sensor evaluation dispatcher |
| `sensor_acc.F` | Acceleration sensor |
| `dist_node_plane_3n.F` | Distance from node to plane (geometry sensor) |
| `dist_node_seg3n.F`, `dist_node_seg4n.F` | Distance from node to segment |

Sensor types include: force, displacement, velocity, acceleration, energy, time, and user-defined.

## Cross-Section Forces (`sect/`)

Section cuts compute the resultant force and moment acting across a user-defined cutting plane through the mesh.

| File | Role |
|------|------|
| `cutmain.F` | Section cut main computation |
| `cutcnt.F` | Count elements crossing the section |
| `cutcon.F` | Connectivity of cut elements |
| `cutfunc.F`, `cutfunce.F` | Force integration across the section |

Activated by `/SECT` keyword; results written to the time history.

## Coordinate Frame Transforms (`skew/`)

| File | Role |
|------|------|
| `movfram.F` | Moving frame (co-rotating with a rigid body) |
| `newskw.F` | New skew frame computation |
| `relfram.F`, `relfram_m1.F` | Relative frame transforms |
| `rotbmr.F` | Rotation matrix for skew frames |

Skew frames allow loads, BCs, and outputs to be expressed in local (material / body) coordinates rather than global Cartesian. Defined by `/SKEW`.

## Seatbelts (`seatbelts/`)

Implements 1D seatbelt / retractor / pretensioner kinematics and guided cable force elements.

| File | Role |
|------|------|
| `kine_seatbelt_force.F` | Seatbelt force from kinematics |
| `kine_seatbelt_vel.F` | Seatbelt velocity update |
| `compute_contact_force_guide.F90` | Contact force for guided cable |
| `guided_cable_force.F90` | Guided cable element force |
| `material_flow.F` | Material flow through retractor / slip-ring |

## Lagrange Multipliers (`lagmul/`)

Generic Lagrange multiplier constraint utilities used by tied interfaces and implicit constraints.

## Universal Joint (`univ/`)

Kinematics for universal (Cardan) joints connecting rigid bodies or beams at fixed-angle connections.

## Accelerometer (`accele/`)

`accel1.F` implements a virtual accelerometer attached to a node or rigid body. Outputs the acceleration in a specified coordinate frame (used for injury criteria assessment in crash simulations).

## Related Documentation

- `engine/source/loads/README.md` — load curves use `curve/finter.F`
- `engine/source/constraints/README.md` — coordinate frames for constraints
- `engine/source/output/README.md` — section forces written to TH output
