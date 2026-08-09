# Common Output Utilities (common_source/output)

This directory provides output-writing utilities shared between the starter and engine. It handles checksums, restart data writing, and the main `.out` text file.

## Directory Structure

```
output/
├── checksum/          — Checksum computation for restart integrity
├── restart/           — Shared restart file write routines (ALE grid, BCS, etc.)
└── write_out_file.F90 — Main `.out` text file writer
```

## Checksum (`checksum/`)

| File | Role |
|------|------|
| `checksum.cpp` | Compute CRC/checksum of restart file blocks |

The checksum is stored in the restart file header. When a run restarts, the engine verifies the checksum matches to detect file corruption or version mismatch.

## Shared Restart Write (`restart/`)

Some restart data categories are written by both the starter (for initial restart) and the engine (for continuation restarts):

| File | Data written |
|------|-------------|
| `write_ale_grid.F90` | ALE grid node positions and velocities |
| `write_bcs_nrf.F90` | Non-reflecting boundary condition state |
| `write_bcs_wall.F90` | Wall boundary condition state |

These routines are in `common_source/` rather than `engine/` or `starter/` because they are called from both contexts with the same format.

## Main Output File (`write_out_file.F90`)

`write_out_file.F90` provides the shared logic for writing entries to the main text output file (`.out`). This includes:
- Section headers and separators
- Formatted floating-point output with units
- Error and warning message insertion

Both the starter (model summary) and engine (run progress, energy balance) use this module.

## Related Documentation

- `engine/source/output/README.md` — engine-specific output (H3D, TH, anim, etc.)
- `common_source/qa/README.md` — QA-check output (uses write_out_file)
