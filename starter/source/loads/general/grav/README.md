# Starter Gravity Load (`starter/source/loads/general/grav/`)

Reads `/GRAV` (gravity body force) input and writes parameters to restart.

## Key Files

| File | Role |
|------|------|
| `hm_read_grav.F` | Read `/GRAV` card: direction vector `(gx, gy, gz)`, scale factor, function reference |
| `hm_preread_grav.F` | Pre-read pass: count gravity loads for array allocation |

## Related Documentation

- `starter/source/loads/general/README.md` — parent directory
- `engine/source/loads/general/grav/README.md` — engine gravity application
