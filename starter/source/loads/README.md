# Starter Loads Subsystem

This subsystem reads and validates all load keyword definitions, then initialises load data structures for the restart file.

## Directory Structure

```
loads/
├── general/       — Standard loads (force, pressure, gravity, centrifugal, …)
│   ├── cload/     — Concentrated nodal force (/CLOAD)
│   ├── grav/      — Gravitational load (/GRAV)
│   ├── load_centri/ — Centrifugal force (/CENTRI)
│   ├── load_pcyl/ — Cylindrical pressure (/PCYL)
│   ├── load_pressure/ — Distributed pressure (/PLOAD)
│   ├── pfluid/    — Fluid pressure (/PFLUID)
│   ├── pload/     — General pressure (/PLOAD)
│   └── preload/   — Pre-loading (prestress)
├── bem/           — Boundary Element Method load (acoustic FSI)
├── bolt/          — Bolt pretension load (/BOLT/PRET)
├── laser/         — Laser load (/LASER)
└── pblast/        — Pressure blast (/PBLAST)
```

## Key Operations Per Load Type

### Concentrated Force (`general/cload/`)
Reads `/CLOAD` parameters (force components, load curve reference, node group) and stores force vectors for the engine.

### Gravity (`general/grav/`)
Reads `/GRAV` (direction and magnitude). Computes the initial gravity-induced nodal forces stored as reference load in the restart.

### Bolt Pretension (`bolt/`)
`/BOLT/PRET` models a pre-tensioned bolt:
- `sboltini.F` — reads bolt geometry (node pairs, axial direction)
- `iniboltprel.F` — computes initial bolt preload and section area
- `sectarea.F` — computes bolt cross-section area

The bolt pretension creates initial internal forces that balance at the start of the simulation.

### BEM Load (`bem/`)
Reads the acoustic Boundary Element Method interface (`/BEM`) — the wet surface definition for underwater shock FSI. `hm_read_bem.F` reads this from HyperMesh format.

### Laser and Blast
Mirror the engine-side definitions (see `engine/source/loads/README.md`). The starter reads and validates parameters; the engine evaluates them each step.

## Function References

All time-varying loads reference a `/FUNCT` function curve. The starter validates that referenced function IDs exist and stores the function index in the load data structure.

## Related Documentation

- `engine/source/loads/README.md` — runtime load evaluation (engine side)
- `starter/source/tools/README.md` — function validation utilities used here
