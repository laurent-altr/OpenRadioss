# Starter Thermal Loads (`starter/source/loads/thermic/`)

Reads thermal boundary conditions: convection and radiation heat transfer on structural surfaces.

## Key Files

| File | Role |
|------|------|
| `hm_read_convec.F` | Read `/CONVEC` convective heat transfer: segment set, convection coefficient, ambient temperature |
| `hm_read_radiation.F` | Read `/RADIATION` radiative heat transfer: emissivity, view factor, environment temperature |
| `hm_preread_convec.F` | Pre-read pass for convection array sizing |
| `hm_preread_radiation.F` | Pre-read pass for radiation array sizing |

## Related Documentation

- `starter/source/loads/README.md` — parent loads directory
- `engine/source/constraints/thermic/README.md` — thermal constraints in engine
