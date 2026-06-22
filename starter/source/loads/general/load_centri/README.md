# Starter Centrifugal Load (`starter/source/loads/general/load_centri/`)

Reads `/CENTRI` centrifugal/Coriolis load input.

## Key Files

| File | Role |
|------|------|
| `hm_read_load_centri.F` | Read `/CENTRI` card: axis of rotation, angular velocity, node set |
| `hm_preread_load_centri.F` | Pre-read pass for array sizing |

## Related Documentation

- `starter/source/loads/general/README.md` — parent directory
- `engine/source/loads/general/load_centri/README.md` — engine centrifugal application
