# TYPE24 Interface — Surface to Surface (`starter/source/interfaces/int24/`)

Starter reader for /INTER/TYPE24: surface-to-surface contact, the recommended general contact for crashworthiness simulations.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type24.F` | Parse /INTER/TYPE24 card parameters |
| `arret_message.F` | Fatal error message for invalid TYPE24 configuration |

## Description

TYPE24 is the primary production contact type in Radioss. It uses a voxel-based broad phase (inherited from TYPE7) combined with a surface-to-surface narrow phase that checks both master nodes against slave surface and slave nodes against master surface (symmetric). This eliminates the spurious penetrations that occur in pure node-to-surface formulations when the master mesh is coarser. Supports segment thickness, thickness variations, and edge-to-segment sub-contact.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int07/README.md` — TYPE7 general node-to-surface
- `starter/source/interfaces/int25/README.md` — TYPE25 with edge contact
- `engine/source/interfaces/README.md` — runtime contact enforcement
