# TYPE21 Interface — Self-Contact (`starter/source/interfaces/int21/`)

Starter reader for /INTER/TYPE21: self-contact (a surface contacting itself), used for folding and crushing simulations.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type21.F` | Parse /INTER/TYPE21 card parameters |

## Description

TYPE21 enables a single segment set to detect and resist self-penetration. The engine's broad-phase voxel search (`intsort`) identifies potentially contacting segments within the same surface; the narrow phase then computes gap and applies penalty forces.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int07/README.md` — TYPE7 general contact
