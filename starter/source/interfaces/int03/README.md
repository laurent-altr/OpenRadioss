# TYPE3 Interface — Tied Segments to Segments (`starter/source/interfaces/int03/`)

Starter reader for /INTER/TYPE3: segment-to-segment tied contact.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type03.F` | Parse /INTER/TYPE3 card parameters |

## Description

TYPE3 ties master and slave segment sets together, distributing constraint forces through segment shape functions. Used when two surface meshes must be joined non-conformally (differing mesh sizes on the interface).

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
