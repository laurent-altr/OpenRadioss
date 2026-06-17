# Solid Properties (`starter/source/properties/solid/`)

Reads solid (3D continuum) element property types.

## Key Files

| File | Property | Description |
|------|----------|-------------|
| `hm_read_prop06.F` | TYPE6 | Standard 8-node solid (1-point reduced integration) |
| `hm_read_prop14.F` | TYPE14 | Solid with 2×2×2 full Gauss integration |
| `hm_read_prop15.F` | TYPE15 | Solid with physical stabilisation (hourglass) |
| `hm_read_prop43.F` | TYPE43 | Composite solid (multi-layer brick) |

## Key Parameters

- Integration rule (reduced/full/selective)
- Hourglass control type and coefficient `Qhg`
- Bulk viscosity coefficients `q1`, `q2` (linear and quadratic)
- Strain formulation (incremental or total)
- Output integration point selection

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `engine/source/elements/README.md` — solid formulations
