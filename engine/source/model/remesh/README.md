# Engine Adaptive Remesh (`engine/source/model/remesh/`)

Applies adaptive mesh refinement (AMR) during the simulation: splits distorted elements and transfers solution to the refined mesh.

## Key Files

| File | Role |
|------|------|
| `admdiv.F` | Adaptive mesh division: split element into sub-elements |
| `admerr.F` | Error indicator: compute per-element refinement criterion |
| `admfor0.F` | Force transfer to remeshed nodes |
| `admgvid.F` | Ghost node update after remeshing |
| `admini.F` | Initialise adaptive mesh data structures |
| `sh_offset_mod.F90` | Shell offset correction after refinement |

## Algorithm

Adaptive refinement is triggered when `admerr.F` detects that an element's error indicator (e.g., effective plastic strain gradient, contact penetration) exceeds the refinement threshold. `admdiv.F` splits the flagged element (1→4 for shells, 1→8 for hexahedra), interpolating the solution (stress, history) to new integration points. `admfor0.F` transfers nodal forces from the old coarse mesh to the refined mesh nodes before the next step.

## Related Documentation

- `engine/source/README.md` — engine architecture
- `starter/source/model/remesh/README.md` — adaptive mesh setup in starter
