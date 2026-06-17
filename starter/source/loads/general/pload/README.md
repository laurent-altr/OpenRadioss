# Starter PLOAD (`starter/source/loads/general/pload/`)

Reads `/PLOAD` applied pressure on element faces (element-based pressure, as opposed to segment-set-based `/LOAD/PRES`).

## Key Files

| File | Role |
|------|------|
| `hm_read_pload.F` | Read `/PLOAD` card: element set, face number, pressure magnitude |
| `hm_preread_pload.F` | Pre-read pass for array sizing |

## Description

`/PLOAD` applies pressure directly to element face IDs rather than a segment set. More convenient for simple structured meshes where face numbering is unambiguous. The reader converts element-face references to segment-node lists for the engine.

## Related Documentation

- `starter/source/loads/general/README.md` — parent directory
- `starter/source/loads/general/load_pressure/README.md` — segment-set pressure
