# Loads Subsystem

This subsystem applies external forces, pressures, body forces, and other loading conditions to the model during the engine time loop.

## Directory Structure

```
loads/
├── general/       — Standard load types (force, pressure, gravity, centrifugal, …)
├── laser/         — Laser energy deposition load
└── pblast/        — Pressure blast load (incident wave, ConWep)
```

## General Loads (`general/`)

| Directory / File | Keyword | Description |
|-----------------|---------|-------------|
| `force.F90` | `/CLOAD` | Concentrated nodal force |
| `force_imp.F` | `/CLOAD` | Concentrated force for implicit solver |
| `forcefingeo.F` | `/CLOAD` | Force with finite geometry correction |
| `forcepinch.F` | `/CLOAD` | Force on pinch (beam offset) nodes |
| `grav/` | `/GRAV` | Gravitational body force |
| `inivel/` | `/INIVEL` | Initial velocity application |
| `load_centri/` | `/CENTRI` | Centrifugal body force |
| `load_pcyl/` | `/PCYL` | Cylindrical pressure load |
| `load_pressure/` | `/PLOAD` | Distributed pressure on surfaces |
| `pfluid/` | `/PFLUID` | Fluid pressure load |

### Pressure Load (`load_pressure/`)

Applies a pressure normal to element surfaces. The pressure can vary in time via a load curve (`/FUNCT`). Supports solid element faces, shell surfaces, and segment sets.

### Gravity (`grav/`)

Computes and applies gravitational body force `F = m·g`. The direction and magnitude are defined by `/GRAV`. Applied as an equivalent nodal force in the assembly step.

### Initial Velocity (`inivel/`)

`/INIVEL` sets nodal velocities at the start of the simulation (or at restart). This is technically a load only at time zero; thereafter, velocities evolve from the equations of motion.

### Centrifugal Force (`load_centri/`)

Applies fictitious centrifugal and Coriolis body forces for rotating frame analyses.

## Laser Load (`laser/`)

Simulates energy deposition from a laser beam onto the model surface.

| File | Role |
|------|------|
| `laser1.F` | Laser beam geometry and energy distribution |
| `laser2.F` | Energy → temperature coupling on target surface |

The laser deposits energy as a volumetric heat source or surface heat flux, depending on the material absorption model.

## Pressure Blast (`pblast/`)

Implements incident pressure wave loads from explosive events.

| File | Role |
|------|------|
| `pblast.F` | Blast pressure main computation |
| `pblast_1.F` | Wave arrival time and peak pressure (ConWep / Kingery-Bulmash) |
| `pblast_2.F` | Pressure decay curve |
| `pblast_3.F` | Reflection factor computation |

The blast load computes the overpressure at each exposed surface node as a function of the scaled distance from the charge, using empirically fitted curves (ConWep method).

Activated by `/PLOAD/PBLAST` or `/LOAD/BLAST`.

## Load Curves

All time-varying loads reference a function (`/FUNCT`) that scales the load magnitude as a function of time. The function look-up is performed centrally in the `tools/` subsystem.

## Related Documentation

- `engine/source/boundary_conditions/README.md` — kinematic BCs (velocity / displacement constraints)
- `engine/source/assembly/README.md` — how load forces are added to the nodal force vector
