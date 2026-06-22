# DYNAIN Output (`engine/source/output/dynain/`)

Writes DYNAIN files: an ASCII OpenRadioss/LS-DYNA compatible output of the deformed mesh with updated material state for use as input to a subsequent analysis.

## Key Files

| File | Role |
|------|------|
| `gendynain.F` | Master DYNAIN writer: dispatch to per-type routines |
| `dynain_node.F` | Write nodal coordinates and velocities |
| `dynain_shel_mp.F` | Write shell element data (multi-processing) |
| `dynain_shel_spmd.F` | Write shell element data (SPMD gather) |
| `dynain_c_strag.F` | Write solid element strain (global) to DYNAIN |
| `dynain_c_strsg.F` | Write solid element stress (global) to DYNAIN |
| `dynain_size.F` | Estimate buffer size needed for DYNAIN |
| `read_dynain.F` | Read DYNAIN file (for restart or transfer) |
| `shell_rota.F` | Shell rotation matrix update for DYNAIN |

## Use Cases

DYNAIN is used for:
- **Springback analysis**: transfer the deformed metal-forming geometry and stress state to a subsequent implicit springback step
- **Multi-stage forming**: the deformed mesh from one stamp becomes the input to the next
- **Crash-after-forming**: stamped part (with residual stresses) used as crash model initial state

## Related Documentation

- `engine/source/output/README.md` — all output types
- `engine/source/output/restart/README.md` — restart (different from DYNAIN)
