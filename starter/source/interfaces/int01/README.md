# TYPE1 Interface — Tied Nodes to Surface (`starter/source/interfaces/int01/`)

Starter reader for /INTER/TYPE1: tied node-to-surface contact where slave nodes are permanently constrained to remain on the master surface.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type01.F` | Parse /INTER/TYPE1 card parameters |
| `i1bcs_check.F90` | Check boundary conditions compatibility for tied nodes |
| `inter1_check_ale_lag_sides.F90` | Verify ALE/Lagrangian surface assignment |
| `inter1_seg_utils.F90` | Segment set utilities for TYPE1 master surface |

## Description

TYPE1 enforces a permanent kinematic tie between slave nodes and the master surface. The starter reads the slave node set and master segment set, checks for geometric overlap and BC compatibility, and writes the constraint data to the restart file. The engine enforces the tie via Lagrange multipliers or penalty at each step.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `engine/source/interfaces/README.md` — runtime contact enforcement
