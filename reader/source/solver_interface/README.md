# Solver Interface (`reader/source/solver_interface/`)

Fortran/C interface between the OpenRadioss reader library and the solver binaries: exposes model data from the SDI layer to Fortran engine/starter code.

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `source/` | Interface implementation: Fortran-callable C wrappers that translate SDI object references to Fortran array data |

## Architecture

The solver interface bridges two worlds:
- The C++ SDI layer (object-oriented model database)
- The Fortran solver code (array-based data structures)

Wrapper functions follow the pattern:
```c
void get_node_coords_(int* node_id, double* x, double* y, double* z)
```

called from Fortran as:
```fortran
call get_node_coords(node_id, x, y, z)
```

This interface is used by the Radioss reader binary to extract model data for writing starter restart files, bypassing the full starter parsing pipeline for models already held in memory by HyperMesh.

## Related Documentation

- `reader/source/sdi/README.md` — SDI model data interface
- `reader/source/README.md` — reader architecture overview
