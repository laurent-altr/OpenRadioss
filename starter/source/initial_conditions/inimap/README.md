# Initial State Mapping (`starter/source/initial_conditions/inimap/`)

Reads and maps an initial field (stress, strain, velocity) from an external 1D or 2D data file onto the mesh via /INIMAP1D and /INIMAP2D.

## Key Files

| File | Role |
|------|------|
| `hm_read_inimap1d.F` | Parse /INIMAP1D card: 1D profile along a path |
| `hm_read_inimap2d.F` | Parse /INIMAP2D card: 2D field on a surface |
| `lec_inimap1d_file.F` | Read 1D data file (position → field value pairs) |
| `lec_inimap2d_file.F` | Read 2D data file (x,y → field value table) |
| `ini_inimap1d.F` | Interpolate 1D profile onto mesh nodes/elements |
| `multi_solve_eint.F90` | Multi-phase internal energy solver for mapped EOS state |

## Description

INIMAP allows importing pre-existing field distributions (e.g., residual stress from a forming simulation, hydrostatic pressure from a CFD run) onto the Radioss mesh. 1D profiles are interpolated along a user-defined axis; 2D maps use bilinear interpolation on a structured table. `multi_solve_eint.F90` solves for consistent specific internal energy when the mapped pressure and density imply a non-trivial EOS state.

## Related Documentation

- `starter/source/initial_conditions/README.md` — parent directory
- `starter/source/initial_conditions/inista/README.md` — initial stress from Y-file
