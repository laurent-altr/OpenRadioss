# TYPE22 Interface — Segment to Segment (Mortar) (`starter/source/interfaces/int22/`)

Starter reader for /INTER/TYPE22: segment-to-segment mortar contact (symmetric, no master/slave distinction).

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type22.F` | Parse /INTER/TYPE22 card parameters |

## Description

TYPE22 implements symmetric mortar contact where both surfaces are treated equally (no master/slave bias). The contact pressure is distributed through mortar integrals across overlapping segment pairs, providing more accurate pressure distributions than TYPE7 for non-conformal meshes.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int25/README.md` — TYPE25 surface-to-surface
