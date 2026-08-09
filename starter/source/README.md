# OpenRadioss Starter — Source Architecture

The starter binary reads an OpenRadioss input deck (`.rad` / `_0000.rad`), validates the model, initialises all data structures, and writes a binary restart file (`_0001.rad`) consumed by the engine. It performs no time integration.

## Entry Points

- **`starter/starter.F`**: Main program. Parses arguments, sets up MPI, calls `LECTUR` then writes restart.
- **`starter/lectur.F`**: Top-level input parser. Reads keyword blocks in sequence from the input deck.
- **`starter/contrl.F`**: Control logic connecting the parsed model to output writers.

## Processing Pipeline

```
Input deck (.rad)
      │
      ▼
  LECTUR          — keyword-by-keyword parsing
      │
      ▼
  Validation      — consistency / compatibility checks
      │
      ▼
  Initialisation  — compute derived quantities (masses, normals, element data)
      │
      ▼
  Restart write   — _0001.rad binary restart (engine reads this)
      │
      ▼
  Starter output  — _0000.out summary file, H3D/anim initial state
```

## Subsystem Map

| Directory | Purpose |
|-----------|---------|
| `airbag/` | Airbag model initialisation (control volume, FV) |
| `ale/` | ALE / Euler model setup |
| `ams/` | Advanced Mass Scaling initialisation |
| `boundary_conditions/` | BCS / EBCS keyword parsing and setup |
| `constraints/` | Rigid body, rigid wall, kinematic link setup |
| `coupling/` | External coupling adapter initialisation |
| `devtools/` | Developer utilities and debugging helpers |
| `elements/` | Element initialisation (shell, solid, beam, spring, SPH, …) |
| `fluid/` | Fluid model setup |
| `general_controls/` | Global control keyword parsing (`/CONTROL`, `/RUN`, …) |
| `groups/` | Node and element group reading (`/GRNOD`, `/GRSH`, …) |
| `implicit/` | Implicit solver parameter setup |
| `initial_conditions/` | Initial velocity, temperature, stress (`/INIVEL`, …) |
| `interfaces/` | Contact interface parsing and setup (TYPE7–TYPE25, …) |
| `loads/` | Load curve and load application setup |
| `materials/` | Material keyword parsing and property tables |
| `model/` | Global model data layout and memory allocation |
| `modules/` | Fortran module definitions shared across the starter |
| `multifluid/` | Multi-fluid model setup |
| `output/` | Starter output writers (`.out` file, initial H3D, animations) |
| `properties/` | Property/section keyword parsing (`/PROP`, …) |
| `restart/` | Restart file writing logic |
| `spmd/` | MPI domain decomposition and partitioning |
| `stack/` | Memory stack management |
| `starter/` | Main entry point and top-level control |
| `system/` | System utilities (CPU time, file I/O helpers) |
| `tools/` | Miscellaneous utilities |
| `user_interface/` | User-defined material and property hooks |

## Restart File Format

The binary `_0001.rad` file is the primary contract between starter and engine. It contains:
- All element connectivity and node coordinates
- Material and property tables (evaluated at time 0)
- Contact interface data
- Group and set definitions
- Initial conditions

The file format is internal to OpenRadioss; it is versioned by build number.

## Key Differences from the Engine

| Aspect | Starter | Engine |
|--------|---------|--------|
| Purpose | Parse & initialise | Time integrate |
| Time loop | None | Explicit/implicit loop |
| MPI use | Domain decomposition only | Full SPMD exchange each step |
| Output | `.out` summary, initial state | H3D, TH, animation at intervals |

## Related Documentation

- Root `HOWTO.md` — build instructions
- Root `INSTALL.md` — how to run OpenRadioss (starter + engine arguments)
- `.github/copilot-instructions.md` — Fortran coding standards
