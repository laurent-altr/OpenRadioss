# XFEM Element Initialisation (`starter/source/elements/xelem/`)

Starter initialisation for XFEM (eXtended Finite Element Method) enriched shell elements used for crack propagation.

## Key Files

| File | Role |
|------|------|
| `xinit3.F` | Main XFEM element initialisation |
| `xgrhead.F` | Write XFEM element group header to restart |
| `xgrtails.F` | Write XFEM element group tail data to restart |
| `xini28.F` | Initialise XFEM enrichment type 28 |
| `xini29.F` | Initialise XFEM enrichment type 29 |
| `xini30.F` | Initialise XFEM enrichment type 30 |
| `xini31.F` | Initialise XFEM enrichment type 31 |

## Description

XFEM augments standard shell elements with discontinuous enrichment functions that represent crack geometry without remeshing. `xinit3.F` sets up the enriched DOF arrays and initial level-set field. The `xiniNN.F` routines handle different enrichment variants (Heaviside, branch functions) for different crack configurations.

## Related Documentation

- `starter/source/elements/README.md` — parent directory
- `starter/source/initial_conditions/inicrack/README.md` — initial crack definition
- `engine/source/elements/shell/README.md` — engine shell integration with XFEM
