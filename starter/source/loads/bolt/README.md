# Bolt Preload (`starter/source/loads/bolt/`)

Reads and initialises /BOLT pre-tension loads for bolted joint simulation.

## Key Files

| File | Role |
|------|------|
| `iniboltprel.F` | Initialise bolt pre-tension state (compute initial stress from clamping force) |
| `sboltini.F` | Bolt SPMD initialisation |
| `sectarea.F` | Compute bolt shank cross-sectional area for stress calculation |

## Description

`/BOLT` applies a pre-tension force to a bolt shank section. `iniboltprel.F` converts the target clamping force to an initial axial stress in the bolt elements using the shank area from `sectarea.F`. The pre-tensioned state is written to the restart file as an initial stress condition.

## Related Documentation

- `starter/source/loads/README.md` — parent directory
