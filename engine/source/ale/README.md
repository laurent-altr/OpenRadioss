# ALE / Euler / Multi-Material Subsystem

This subsystem implements Arbitrary Lagrangian-Eulerian (ALE) and pure Eulerian formulations. It is activated when `/ALE` or `/EULER` keywords appear in the input deck.

## What is ALE?

In a standard Lagrangian computation, the mesh moves with the material. ALE decouples material and mesh motion: after each Lagrangian step the mesh is re-positioned (rezoned) and the solution is remapped (advected) onto the new mesh. Pure Euler fixes the mesh entirely and advects all material quantities through it.

## Directory Structure

```
ale/
├── ale2d/         — 2D ALE formulation (axisymmetric / plane strain)
├── ale3d/         — 3D ALE core (convection, flux, rezone)
├── ale51/         — ALE type-51 higher-order elements
├── alefvm/        — ALE Finite Volume Method variant
├── alemuscl/      — MUSCL (Monotone Upstream-centered) higher-order advection
├── bimat/         — Bi-material (two-fluid) interface tracking
├── euler2d/       — Pure 2D Eulerian solver
├── euler3d/       — Pure 3D Eulerian solver
├── grid/          — Mesh rezoning / smoothing strategies
├── inter/         — ALE interface / fluid-structure coupling
├── porous/        — Porous media ALE
├── subcycling/    — ALE sub-cycling for stability
├── turbulence/    — Turbulence models for ALE/Euler flows
├── alemain.F      — ALE main loop driver
├── init_ale*.F90  — ALE initialisation routines
├── arezon.F90     — Rezoning dispatcher
├── aconve.F90     — Advection (convection) dispatcher
├── atherm.F       — Thermal ALE step
└── alethe.F       — ALE thermal advection
```

## Processing Sequence (per time step)

Each ALE cycle follows a Lagrangian step + ALE step split:

```
1. Lagrangian step        — standard element force computation (elements/)
2. Grid rezoning          — move mesh nodes (grid/, arezon.F90)
3. Advection/transport    — remap solution to new mesh (ale3d/, alemuscl/, aconve.F90)
     ├── Flux computation    — ale3d/a4flux3.F, aconve.F90
     ├── MUSCL reconstruction — alemuscl/alemuscl_upwind.F (high-order, monotone)
     └── Gradient recovery   — ale3d/agrad3.F
4. Interface tracking     — bimat/, inter/ (for multi-material)
5. Thermal update         — atherm.F / alethe.F (if thermal coupling active)
```

## Key Algorithms

| Algorithm | Location | Description |
|-----------|----------|-------------|
| Van Leer MUSCL | `alemuscl/` | 2nd-order monotone advection limiter |
| Upwind advection | `ale3d/aconv3.F` | 1st-order donor-cell scheme |
| Winslow smoothing | `grid/` | Mesh smoothing for rezoning |
| HALE rezoning | `ale51/` | Higher-order ALE element formulation |
| FVM fluxes | `alefvm/` | Finite Volume Method flux computation |

## Euler Solver (`euler3d/`)

The pure Euler formulation fixes the mesh. Only fluxes are computed:
- `eflux3.F` — Eulerian flux (density, momentum, energy)
- `egrad3.F` — Eulerian gradient computation

## Multi-Material (`bimat/`)

Tracks the interface between two materials sharing ALE elements. Uses marker or VOF-like techniques to maintain material fractions per cell.

## Porous Media (`porous/`)

Implements Darcy-like flow through porous ALE elements. Used for airbag fabric porosity and similar applications.

## Subcycling (`subcycling/`)

Allows the ALE advection step to run at a different (larger) time step than the Lagrangian step, reducing computational cost when advection stability allows.

## ALE + Fluid-Structure Interaction (`inter/`)

The `inter/` subdirectory handles the coupling between the ALE/Euler fluid domain and Lagrangian structural elements. Forces and velocities are exchanged at the FSI boundary.

## Related Documentation

- `engine/source/elements/README.md` — Lagrangian element step
- `engine/source/assembly/README.md` — force assembly used in the Lagrangian step
- `common_source/eos/README.md` — EOS models required by ALE/Euler materials
