# engine/source/properties/

## Purpose
Engine-side property computation for composite (laminated) elements. Handles
inter-ply delamination between composite layers during the simulation.

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `composite_options/stack/` | Inter-ply delamination for stacked composite shells: `DELAMINATION` (main), `DELM01LAW`, `DELM02LAW`, `DELM24LAW` (failure law variants), `CUPDT_PLY` (ply coordinate update), `CNDT_PLY` (ply normal update), `CBADEF_PLY` / `CBAFINT_PLY` / `CBAPROJ_PLY` / `CBAVIT_PLY` (deformation, force integration, projection, velocity update for ply stacks) |

## Context
Composite property routines are called from the shell element kernels
(`CFORC3`, `CBAFORC3`) when `/PROP/STACK` or composite lay-up is active.
Delamination failure is tracked in `ELBUF_TAB(NG)%INTLAY(ILAY)` (inter-ply layer
buffers; see `doc/ELBUF_TAB_documentation.md` §3).

## Dependencies
- Called by: shell element kernels in `engine/source/elements/shell/`
- Uses: `ELBUF_STRUCT_` `intlay` field and `prop_param_` from `common_source/modules/mat_elem/`
