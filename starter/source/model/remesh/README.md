# Adaptive Mesh Refinement Setup (`starter/source/model/remesh/`)

Reads and initialises `/ADAP`/`/ADMESH` adaptive mesh refinement (AMR) definitions for use by the engine.

## Key Files

| File | Role |
|------|------|
| `build_admesh.F` | Build the adaptive mesh data structures from the refinement region definition |
| `set_admesh.F` | Set the initial AMR region flags on elements |
| `state_admesh.F` | Initialise the AMR state (refinement level, parent-child relationships) |
| `admlist.F` | Build element list for AMR regions |
| `admbcs.F` | Apply boundary conditions to newly created nodes after refinement |
| `nbadmesh.F` | Count elements in adaptive mesh regions |

## AMR in OpenRadioss

The engine refines elements during simulation when a refinement criterion is met (e.g., high plastic strain, crack tip proximity). The starter prepares:
1. The initial refinement level map
2. Parent-child element tables (for interpolating state to refined children)
3. Boundary condition inheritance rules (how BCs are applied to new nodes)

The actual refinement (`admdiv.F` in `engine/source/model/`) is performed by the engine.

## Related Documentation

- `starter/source/model/README.md` — parent model directory
- `engine/source/model/README.md` — engine AMR execution (admdiv, admerr)
