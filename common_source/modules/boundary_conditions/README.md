# Boundary Condition Modules (`common_source/modules/boundary_conditions/`)

Fortran 90 modules defining data types for boundary conditions. Shared between starter and engine.

## Modules

| File | Module | Contents |
|------|--------|---------|
| `bcs_mod.F90` | `BCS_MOD` | Standard `BCS` data: node lists, DOF flags, function IDs for prescribed BC |
| `ebcs_mod.F90` | `EBCS_MOD` | Extended BCS (`EBCS`) data: absorbing, cyclic, flow, propellant BCs |

## BCS_MOD

Stores the standard boundary condition table:
- Node or node-group reference
- Constrained DOF bitmask (X, Y, Z translation; X, Y, Z rotation)
- BC type (fixed / prescribed velocity / prescribed displacement)
- Function ID for time-varying BCs

## EBCS_MOD

Stores extended boundary conditions applied to element faces rather than nodes:
- Surface group reference (element face list)
- BC type code (TYPE4=absorbing, TYPE5=inlet/outlet, TYPE8=cyclic, etc.)
- Parameters specific to each type

## Related Documentation

- `common_source/modules/README.md` — parent modules directory
- `engine/source/boundary_conditions/README.md` — EBCS implementation
- `engine/source/constraints/general/bcs/README.md` — standard BCS application
