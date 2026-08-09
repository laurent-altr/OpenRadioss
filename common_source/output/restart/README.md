# Common Restart Utilities (`common_source/output/restart/`)

Shared restart read/write routines used by both the engine and starter for serialising model-level data structures.

## Key Files

| File | Role |
|------|------|
| `write_ale_grid.F90` | Write ALE grid coordinates and connectivity to restart |
| `write_bcs_nrf.F90` | Write non-reflecting boundary condition state to restart |
| `write_bcs_wall.F90` | Write rigid wall penalty state to restart |

## Description

These routines are shared between the engine restart writer (`engine/source/output/restart/`) and the starter output because the data structures are defined in `common_source/modules/`. Writing to restart format from both binaries (starter for initial state, engine for checkpoint) is the common pattern.

`write_ale_grid.F90` serialises the ALE mesh node positions after rezoning; `write_bcs_nrf.F90` serialises the NRF buffer state; `write_bcs_wall.F90` serialises penalty rigid wall gap/velocity history needed to resume contact correctly from a checkpoint.

## Related Documentation

- `engine/source/output/restart/README.md` — engine restart output
- `common_source/output/README.md` — parent directory
