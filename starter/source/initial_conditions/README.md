# Initial Conditions (Starter)

This subsystem processes all `/INIVEL`, `/INIGRAV`, and related keywords that set the model state at time zero. Initial conditions are applied after the model geometry is built and before the restart file is written.

## Directory Structure

```
initial_conditions/
├── general/        — General initial conditions (/INIVEL, /INISTA, /INIMAP)
├── detonation/     — Detonation front initialisation (/INIDETONATION)
├── inicrack/       — Initial crack definitions (for XFEM and fracture)
├── inigrav/        — Gravity initialisation (/INIGRAV)
├── inimap/         — Field mapping from external file (/INIMAP)
├── inista/         — Initial stress/strain state (/INISTA)
├── inivol/         — Initial volume fraction (for ALE/multi-fluid)
└── thermic/        — Thermal initial conditions (temperature field)
```

## Initial Velocity (`general/`, `/INIVEL`)

Assigns initial velocity to a group of nodes. Supports:
- Translational velocity (`VX`, `VY`, `VZ`)
- Rotational velocity (angular velocity about an axis)
- Submodel reference frame transformation
- Node-group or part-group specification

## Initial Stress/Strain (`inista/`, `/INISTA`)

Assigns pre-existing stress state to elements at time zero. Used for:
- Pre-loaded structures (pre-stressed concrete, pre-tensioned cables)
- Continuation of a forming simulation without using DYNAIN

## Field Mapping (`inimap/`, `/INIMAP`)

Maps a result field (stress, strain, temperature) from a reference mesh (typically a previous simulation) onto the current mesh by interpolation. The meshes need not be identical.

## Initial Crack (`inicrack/`)

For XFEM fracture simulations, defines the initial crack geometry (crack plane, front location) at time zero. The XFEM enrichment is applied to elements cut by the initial crack.

## Detonation Front (`detonation/`)

For explosive simulations, the detonation front position at time zero is specified. Elements ahead of the front at `t=0` are initialised in the detonated (product gas) state.

## Gravity (`inigrav/`, `/INIGRAV`)

Applies a static gravity pre-load: the model is first solved for the static equilibrium under gravity, and the resulting stress state is used as the initial condition for the dynamic analysis.

## Initial Volume Fractions (`inivol/`)

For ALE / multi-fluid models, sets the material volume fractions in each cell at time zero. This defines the initial interface positions between fluid species.

## Thermal Initial Conditions (`thermic/`)

Assigns initial temperature field to nodes or elements. Used in thermo-mechanical coupled analyses.

## Related Documentation

- `starter/source/README.md` — starter pipeline; initial conditions are the last step before restart write
- `engine/source/loads/README.md` — applied loads (different from initial conditions)
