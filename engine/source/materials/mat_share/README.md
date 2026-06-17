# Shared Material Routines (`engine/source/materials/mat_share/`)

Dispatch, shared kernels, and common utilities used across all material laws.

## Key Files

| File | Role |
|------|------|
| `mmain.F90` / `mmain8.F` | Main material dispatch: call per-law stress-update routine for standard / 8-point solid |
| `cmain3.F` | Main material dispatch for shell elements (plane-stress laws) |
| `meos8.F` | EOS call wrapper for 8-point solid elements |
| `meint.F` | Material energy integration (strain energy density update) |
| `jacobview_v.F` | Jacobian viewer: compute volume ratio J for large-deformation mapping |
| `fmqviscb.F` | Bulk viscosity pressure (artificial viscosity) for all element types |
| `mdtsph.F` | Material time step for SPH particles |

## Architecture

`mmain.F90` is the hot inner-loop dispatcher. It receives the element buffer (stress, strain, history), the material law ID, and the material parameter array, then calls the corresponding `mNNlaw.F` routine. The dispatch is a Fortran CASE statement over law numbers. After the material update, the stress is rotated back to the global frame and stored in the element buffer.

## Related Documentation

- `engine/source/materials/README.md` — parent materials directory
- `engine/source/materials/mat/README.md` — per-law implementations
