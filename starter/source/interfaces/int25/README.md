# TYPE25 Interface — Surface to Surface with Edges (`starter/source/interfaces/int25/`)

Starter reader for /INTER/TYPE25: surface-to-surface contact with explicit edge-to-surface sub-contact for sharp corners and beam edges.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type25.F` | Parse /INTER/TYPE25 card parameters |
| `i25remlin.F` | Remove linear dependencies in TYPE25 Lagrange multiplier system |

## Description

TYPE25 extends TYPE24 with dedicated edge-to-surface contact segments. Free edges of shell elements (and beam edges) are tracked separately and receive their own gap and penalty treatment, preventing edge penetration artefacts in thin-walled structures. The `i25remlin.F` routine handles the case where edge contact Lagrange multiplier equations become linearly dependent.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int24/README.md` — TYPE24 surface-to-surface
- `starter/source/interfaces/int11/README.md` — TYPE11 edge-to-edge
