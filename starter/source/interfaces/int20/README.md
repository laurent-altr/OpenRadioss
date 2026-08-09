# TYPE20 Interface — Node to Surface (Smooth) (`starter/source/interfaces/int20/`)

Starter reader for /INTER/TYPE20: node-to-surface contact with smooth master surface reconstruction (NURBS-based segment normals).

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type20.F` | Parse /INTER/TYPE20 card parameters |

## Description

TYPE20 is similar to TYPE7 but uses a smoothed master surface (averaging normals across segment boundaries) to eliminate force discontinuities at element edges, producing smoother contact force histories for impact simulations.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int07/README.md` — TYPE7 general contact
