# Initial Crack (`starter/source/initial_conditions/inicrack/`)

Reads /INICRACK definitions that pre-open cracks in XFEM shell meshes at the start of the simulation.

## Key Files

| File | Role |
|------|------|
| `hm_read_inicrack.F` | Parse /INICRACK card: crack path geometry, element set, enrichment parameters |

## Description

`hm_read_inicrack.F` reads crack-path coordinates (line segments or spline) and the associated shell element set. It marks enriched nodes and sets the initial level-set field so that the XFEM crack is already open at `t=0`. The data is written to the restart file and picked up by the engine XFEM routines.

## Related Documentation

- `starter/source/initial_conditions/README.md` — parent directory
- `starter/source/properties/xelem/README.md` — XFEM element property
- `engine/source/elements/shell/` — shell element integration with XFEM enrichment
