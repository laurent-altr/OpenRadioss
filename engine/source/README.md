# OpenRadioss Engine — Source Architecture

The engine binary performs the explicit (and optionally implicit) time integration of the finite element model prepared by the starter. It reads the `_0001.rad` restart file produced by the starter and marches the solution forward in time.

## Entry Points

- **`engine/radioss.F`**: Main program. Parses arguments, sets up MPI/OpenMP, and calls `RESOL`.
- **`engine/resol.F`**: Top-level time loop. Orchestrates all subsystems at each time step.
- **`engine/resol_init.F`**: Initialization before the time loop begins.

## Time Loop Overview

Each explicit cycle in `resol.F` follows this sequence:

1. **Time step control** — compute stable `dt` (`time_step/`)
2. **Kinematics** — update velocities and positions (`assembly/`)
3. **Element forces** — compute internal forces per element type (`elements/`, `materials/`)
4. **Interface forces** — compute contact forces (`interfaces/`)
5. **External forces** — apply loads and boundary conditions (`loads/`, `boundary_conditions/`)
6. **Assembly** — scatter element forces to global nodal force vector (`assembly/`)
7. **Constraint enforcement** — apply rigid bodies, kinematic conditions (`constraints/`)
8. **Output** — write results at requested intervals (`output/`)
9. **ALE rezoning** (if active) — remap solution to new mesh (`ale/`)

## Subsystem Map

| Directory | Purpose |
|-----------|---------|
| `airbag/` | Control-volume and finite-volume airbag models |
| `ale/` | Arbitrary Lagrangian-Eulerian and Euler methods |
| `ams/` | Selective (Advanced) Mass Scaling for large time steps |
| `assembly/` | Nodal force/velocity/acceleration assembly and damping |
| `boundary_conditions/` | Boundary condition enforcement (BCS, EBCS) |
| `constraints/` | Rigid bodies, rigid walls, kinematic links |
| `coupling/` | External solver coupling via preCICE / CWIPI |
| `elements/` | Element formulations (shell, solid, beam, spring, SPH, …) |
| `engine/` | Main entry point, time loop, initialization |
| `fluid/` | Navier-Stokes fluid solver |
| `general_controls/` | Global simulation controls |
| `groups/` | Node and element group management |
| `implicit/` | Implicit time integrator and linear solver (PCG, MUMPS) |
| `input/` | Engine-side input reading (restart, dynain) |
| `interfaces/` | Contact and interface algorithms (TYPE7–TYPE25, …) |
| `loads/` | Applied loads (forces, pressures, blast, laser) |
| `materials/` | Constitutive laws, failure criteria, viscosity |
| `model/` | Global model data structures |
| `modules/` | Fortran module definitions shared across the engine |
| `mpi/` | Domain decomposition and SPMD communication |
| `multifluid/` | Multi-fluid interactions |
| `output/` | Result output (H3D, time history, animation, restart) |
| `properties/` | Section / property type handlers |
| `system/` | System utilities (CPU timing, file management) |
| `time_step/` | Critical time step computation and control |
| `tools/` | Miscellaneous utility routines |
| `user_interface/` | User-defined routines (USERWI, USERMAT, …) |

## MPI / OpenMP Model

- **MPI (SPMD)**: The domain is decomposed into sub-domains at startup. Ghost node layers handle boundary exchanges. All MPI calls are wrapped in `mpi/spmd_mod.F90` (the `SPMD_*` API).
- **OpenMP**: Element loops are parallelised with OpenMP threads. Each thread processes a group of elements.
- See `mpi/README.md` for details.

## Key Data Structures

Defined in `modules/` and used throughout the engine via Fortran `USE` statements:

- `INTBUF_DEF_MOD` — integer working buffers per element type
- `MVSIZ_MOD` — vectorisation block size `MVSIZ`
- `CONSTANT_MOD` — physical and mathematical constants
- `PRECISION_MOD` — working precision kind `WP`

## Related Documentation

- `coupling/README.md` — external solver coupling detail
- Root `HOWTO.md` — how to build the engine
- Root `INSTALL.md` — how to run OpenRadioss
- `.github/copilot-instructions.md` — Fortran coding standards
