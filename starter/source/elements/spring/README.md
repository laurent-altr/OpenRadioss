# Spring Element Initialisation (`starter/source/elements/spring/`)

Reads and initialises spring/damper elements (TYPE4, TYPE8, TYPE12, TYPE13) defined by `/SPRING`.

## Key Files

| File | Role |
|------|------|
| `rinit3.F` | Main spring initialisation: read 2-node connectivity, property, material |
| `rini35.F` | Initialise TYPE3/5 spring (linear stiffness + damping) |
| `rini44.F` | Initialise TYPE4 spring (nonlinear force-displacement curve) |
| `rini46.F` | Initialise TYPE4/6 spring with initial length offset |
| `rkini3.F` | Compute initial spring stiffness from linearised curve slope |
| `rmass.F` | Compute spring nodal mass contribution |
| `rmas12.F` | Mass computation for TYPE12 (general spring with inertia) |
| `rgrhead.F` / `rgrtails.F` | Write spring group header/tail to restart file |
| `rcheckmass.F` | Check spring mass against element time-step requirement |
| `r1buf3.F` / `r2buf3.F` / `r3buf3.F` / `r4buf3.F` | Fill element buffer for TYPE1/2/3/4 springs |
| `r2buf3_law135.F90` | Special buffer fill for spring with LAW135 (visco-elastic) |
| `rgrhead.F` / `rgrtails.F` | Write group header / tail records to restart |

## Spring Types

| TYPE | Behaviour |
|------|----------|
| TYPE3 | Linear elastic spring (stiffness K) |
| TYPE4 | Nonlinear spring (force-displacement function `/FUNCT`) |
| TYPE8 | General elasto-plastic spring with hysteresis |
| TYPE12 | General spring with mass, damping, buckling |
| TYPE13 | Pulley (rope-over-peg) spring |

## Related Documentation

- `starter/source/elements/README.md` — parent directory overview
- `starter/source/properties/spring/` — spring property definitions
- `engine/source/elements/README.md` — spring force computation in engine
