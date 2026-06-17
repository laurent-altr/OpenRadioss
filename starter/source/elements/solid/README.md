# Solid Element Initialisation (`starter/source/elements/solid/`)

Reads and initialises all solid (3D continuum) elements. Each element topology has its own subdirectory:

## Subdirectory Map

| Directory | Topology | Nodes | Integration |
|-----------|---------|-------|-------------|
| `solide/` | Standard hexahedron | 8 | 1-point reduced / full |
| `solide4/` | Tetrahedron | 4 | 1-point |
| `solide8z/` | Hex (z-formulation) | 8 | Hourglass-stabilised |
| `solide10/` | Quadratic tetrahedron | 10 | 4-point |
| `solide20/` | Quadratic hexahedron | 20 | 2×2×2 full |
| `solide6z/` | Pentahedron (wedge) | 6 | 2-point |
| `solidez/` | Hex with enhanced strain | 8 | EAS |
| `solid8p/` | 8-node incompatible mode | 8 | 1-point + bubble |
| `sconnect/` | Surface connector (cohesive zone) | 8 | Traction-separation |

## Common Initialisation Pattern

All solid subdirectories follow the same pattern:

```
suinit3.F / suderi3.F    — read connectivity, property, material
ush_init.F90             — fill element buffer (ELBUF geometry fields)
zerovars_auto.F          — zero stress/strain history variables
ini_mlaw_vars.F          — initialise material law history variables
ini_eos_vars.F           — initialise EOS state variables
r2r_matparam_copy.F      — copy material parameters to element buffer
```

## Key Top-Level Files

| File | Role |
|------|------|
| `get_sort_key_solid.F90` | Sort key for restart ordering of solid element groups |
| `allocbuf_auto.F` | Allocate `ELBUF` arrays for solid elements |
| `elbuf_ini.F` | Initialise element buffer structure |
| `ini_outmax_auto.F` | Initialise max output variables (for `/H3D`, `/ANIM` output) |
| `initvars_auto.F` | Initialise all element state variables to zero |

## IGA Solid

`ige3d/` (see `starter/source/elements/ige3d/`) provides isogeometric (NURBS-based) solid elements that share the same ELBUF infrastructure but use NURBS basis functions.

## Related Documentation

- `engine/source/elements/README.md` — engine solid formulations and force computation
- `starter/source/elements/README.md` — parent directory overview
- `starter/source/properties/solid/` — solid property types (hourglass, strain formulation)
