# Interface Reader (`starter/source/interfaces/reader/`)

Top-level interface input dispatcher: routes each /INTER/TYPE_N card to the appropriate type-specific reader.

## Key Files

| File | Role |
|------|------|
| `hm_read_interfaces.F` | Main dispatcher: loop over /INTER cards, call per-type reader |
| `hm_read_inter_struct.F` | Read interface structural data common to all types |
| `hm_read_inter_fsi.F` | Parse /INTER/FSI: ALE–Lagrangian FSI coupling |
| `hm_read_inter_lagmul.F` | Parse Lagrange multiplier contact options (generic) |
| `inter_dcod_friction.F` | Decode friction model selector |
| `inter_dcod_function.F` | Decode function ID references in contact cards |
| `inter_dcod_sensor.F` | Decode sensor references in contact cards |

## Description

`hm_read_interfaces.F` iterates over all `/INTER` keywords, detects the type number, and delegates to the matching `hm_read_inter_typeNN.F` reader in the corresponding `intNN/` subdirectory. Common structural fields (master/slave segment sets, stiffness parameters, gap) are read by `hm_read_inter_struct.F` and shared across types. Friction, function, and sensor references are decoded by the `inter_dcod_*` routines.

## Related Documentation

- `starter/source/interfaces/README.md` — parent directory overview
- `starter/source/interfaces/int07/README.md` — TYPE7 general contact
- `starter/source/interfaces/int25/README.md` — TYPE25 edge-to-surface
- `starter/source/interfaces/interf1/README.md` — contact geometry initialisation
