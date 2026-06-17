# Starter Tools Subsystem

This subsystem mirrors the engine's `tools/` directory for the starter context: it provides general-purpose utilities for function processing, sensor setup, cross-section definitions, coordinate frames, and other helpers used during model initialisation.

## Directory Structure

```
tools/
├── accele/        — Accelerometer definition and validation
├── activ/         — Element activation (activation/deactivation at t=0)
├── admas/         — Added mass (`/ADMAS`) initialisation
├── curve/         — Load curve validation and function tools
├── lagmul/        — Lagrange multiplier initialisation
├── seatbelts/     — Seatbelt / retractor setup
├── sect/          — Cross-section (section cut) definition
├── sensor/        — Sensor keyword reading and initialisation
├── skew/          — Skew frame reading and setup
├── univ/          — Universal joint setup
└── userwi/        — User wall impact subroutine interface
```

## Load Curve Utilities (`curve/`)

| File | Role |
|------|------|
| `finter.F` | Interpolate function value at a given time (used in starter for validation) |
| `check_function.F` | Validate function table (monotonicity, range, units) |
| `func_inters.F` | Find intersection of two functions |
| `func_maxy.F` | Find maximum Y value of a function |
| `func_slope.F` | Compute slope (derivative) of a function |

## Sensor Setup (`sensor/`)

| File | Role |
|------|------|
| `inisen.F` | Read and initialise `/SENSOR` keyword |
| `iniparsen.F` | Parse sensor parameters |
| `read_sensor_acc.F` | Read accelerometer-type sensor |
| `hm_read_sensors.F` | Read sensors from HyperMesh format |
| `hm_read_sensor_python.F90` | Read Python-defined sensors |

## Cross-Section Setup (`sect/`)

| File | Role |
|------|------|
| `prelecsec.F` | Pre-read section cut parameters |
| `lecsec4bolt.F`, `prelecsec4bolt.F` | Section cut for bolt connectors |
| `hm_read_sect.F` | Read section cut from HyperMesh format |

## Added Mass (`admas/`)

Implements `/ADMAS` — adds non-structural mass to nodes or elements. Used for:
- Modelling equipment or payloads without detailed mesh
- Adjusting mass distribution to match target inertia
- Penalising small elements to avoid DT control issues

## Element Activation (`activ/`)

Controls which elements are active at the start of the simulation. Some elements may begin inactive (deleted) and be re-introduced during the run via sensor-triggered activation.

## User Wall Impact Interface (`userwi/`)

Reads and validates the `/USERWI` keyword that points to a user subroutine for rigid wall impact force calculation.

## Related Documentation

- `engine/source/tools/README.md` — engine-side equivalent (same subdirectory structure)
- `starter/source/README.md` — overall starter pipeline
