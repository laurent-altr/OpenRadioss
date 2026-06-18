# Truss Element Initialisation (`starter/source/elements/truss/`)

Starter initialisation for 2-node truss (bar) elements.

## Key Files

| File | Role |
|------|------|
| `tinit3.F` | Main truss element initialisation |
| `tmass.F` | Compute truss nodal mass |
| `tcoori.F` | Compute truss local coordinate system (axial direction) |
| `tgrhead.F` | Write truss group header to restart |
| `tgrtails.F` | Write truss group tail data to restart |
| `tibuf3.F` | Allocate truss element buffer |
| `tsigini.F` | Initialise truss stress state |
| `dt1lawt.F` | Compute truss element time-step |

## Description

Truss elements carry only axial force. `tcoori.F` sets the unit axial vector and computes the initial length. `tinit3.F` writes the element geometry and material state to the restart file for the engine's truss force computation.

## Related Documentation

- `starter/source/elements/README.md` — parent directory
- `engine/source/elements/truss/README.md` — engine truss integration
