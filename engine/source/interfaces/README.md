# Interfaces (Contact) Subsystem

This subsystem implements all contact and interface algorithms. In OpenRadioss, contact is handled by `/INTER/TYPE<N>` keywords, each corresponding to a subdirectory here.

## Directory Structure

```
interfaces/
├── int07/          — TYPE7: node-to-surface penalty contact (most common)
├── int09/          — TYPE9: node-to-surface tied contact
├── int10/          — TYPE10: tied nodes with failure
├── int11/          — TYPE11: edge-to-edge contact
├── int14/          — TYPE14: surface-to-surface (symmetric penalty)
├── int15/          — TYPE15: tied interface with offset
├── int16/          — TYPE16: node-to-surface (rigid body)
├── int17/          — TYPE17: surface-to-surface penalty (master-slave)
├── int18/          — TYPE18: general tied (with spotwelds)
├── int20/          — TYPE20: rigid wall with given velocity
├── int21/          — TYPE21: rigid wall with sensor
├── int22/          — TYPE22: fluid-structure coupling interface
├── int23/          — TYPE23: SPH-to-surface contact
├── int24/          — TYPE24: edge-to-surface contact
├── int25/          — TYPE25: general segment-based contact
├── inter2d/        — 2D contact interfaces
├── inter3d/        — 3D contact (segment-based, general)
├── interf/         — Shared interface utilities and buffer management
├── intsort/        — Broad-phase sorting and candidate pair detection
├── generic/        — Generic contact utilities (coarse grid, voxel coloring)
└── shell_offset/   — Shell offset correction for contact thickness
```

## Algorithm Overview

Contact handling in the explicit time loop proceeds in two phases:

### 1. Broad Phase (candidate detection) — `intsort/`, `generic/`
- All node/segment pairs within a search radius are identified
- Uses a coarse grid (voxel-based) spatial hash for efficiency
- Output: candidate list `(secondary node, primary segment)` pairs
- Entry point: `inter_box_creation.F`, `inter_cell_color.F`, `inter_check_sort.F`

### 2. Narrow Phase (penetration & force) — `int07/`–`int25/`
- For each candidate pair, check for penetration
- Compute contact force (penalty or Lagrange multiplier)
- Scatter force to secondary node and primary segment nodes
- `i<NN>ass3.F` routines perform the scatter (assembly) step

## TYPE7 — Node-to-Surface Penalty Contact

TYPE7 (`int07/`) is the most widely used contact type. Key files:

| File | Role |
|------|------|
| `i7cor3.F` | Core penetration check and force computation |
| `i7cdcor3.F` | Correction for initial penetrations |
| `i7ass3.F` | Force assembly to nodes |
| `frictionparts_model.F` | Coulomb friction model |

The penalty stiffness is computed from the interface gap and element stiffness.

## TYPE25 — General Segment-Based Contact

TYPE25 (`int25/`) is the most general contact type, supporting:
- Segment-to-segment (edge and surface)
- 1D/2D/3D element mixed surfaces
- Edge-to-edge contact
- Works in SPMD (domain-decomposed) mode

Key entry: `i25_prepare.F` (setup), `i25ass3.F` (assembly).

## INTSORT — Broad-Phase Sorting

The `intsort/` subdirectory implements the spatial sorting strategy. The sorting method (bucket sort, coarse grid, tree) can be selected per interface via `init_interf_sorting_strategy.F` at the top of the `interfaces/` directory.

## SPMD Considerations

Contact across MPI domain boundaries requires ghost node communication. The `interf/` shared utilities manage the interface buffer (`INTBUF_DEF_MOD`) and the communication of contact forces across domain boundaries.

## Related Documentation

- `engine/source/assembly/README.md` — force scatter (assembly) mechanism
- `common_source/interf/README.md` — shared interface buffer structures
- `tools/mockup/contact_sandbox/README.md` — standalone broad-phase mockup for TYPE7
