# TYPE11 Interface — Edge to Edge (`starter/source/interfaces/int11/`)

Starter reader for /INTER/TYPE11: edge-to-edge contact for beams and shell free edges.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type11.F` | Parse /INTER/TYPE11 card parameters |

## Description

TYPE11 handles contact between free edges of shell elements or between beam elements, where node-to-surface contact misses geometric collisions. The starter reads master and slave edge sets, gap, and penalty stiffness.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int25/README.md` — TYPE25 edge-to-surface contact
