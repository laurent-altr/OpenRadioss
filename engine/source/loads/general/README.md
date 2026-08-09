# General Loads (`engine/source/loads/general/`)

Applies general external loads each time step: concentrated forces, pressure, gravity, centrifugal, and initial velocity.

## Top-Level Files

| File | Role |
|------|------|
| `force.F90` | Apply concentrated nodal force (`/CLOAD`): add force vector to global force array |
| `force_imp.F` | Concentrated force for implicit solver |
| `forcefingeo.F` | Concentrated force with finite geometry (follower force — force rotates with node) |
| `forcepinch.F` | Pinch force (closing force applied to opposing surfaces) |
| `load_pressure.F` | Apply uniform pressure to a face (wrapper for subdirectory) |
| `python_call_funct_cload.F90` | Python-defined concentrated load (user-scripted load time history) |

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `grav/` | Gravity body force |
| `inivel/` | Initial velocity (applied as impulse load at t=0) |
| `load_centri/` | Centrifugal and Coriolis body force |
| `load_pcyl/` | Cylindrical pressure load |
| `load_pressure/` | Surface pressure load |
| `pfluid/` | Fluid pressure applied to structural surface |

## Force Application

All loads call into the global force accumulation at the end:
```fortran
F_global(node_dof) = F_global(node_dof) + F_load
```

Time variation: each load references a `/FUNCT` curve (or constant). The scale factor `fscale(t) = funct(t) × user_scale` is evaluated each step.

## Related Documentation

- `engine/source/loads/README.md` — parent loads directory
- `starter/source/loads/README.md` — load reading in starter
