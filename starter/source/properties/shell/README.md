# Shell Properties (`starter/source/properties/shell/`)

Reads shell element property types from keyword input or HM binary.

## Key Files

| File | Property | Description |
|------|----------|-------------|
| `hm_read_prop01.F` | TYPE1 | Isotropic thin shell (Hughes-Liu or Belytschko-Tsay) |
| `hm_read_prop09.F` | TYPE9 | Thin shell with full integration (2×2 Gauss) |
| `hm_read_prop10.F` | TYPE10 | Composite shell (multi-layer laminate) |
| `hm_read_prop11.F` | TYPE11 | Physical stabilisation shell |
| `hm_read_prop16.F` | TYPE16 | Shell with user-defined integration |
| `hm_read_prop17.F` | TYPE17 | Shell with section integration (through-thickness) |
| `hm_read_prop19.F` | TYPE19 | Generalised shell with arbitrary thickness integration |
| `hm_read_prop51.F` | TYPE51 | Shell with enhanced membrane (incompatible modes) |

## Shell Property Parameters

Key parameters stored in the `PM` property array:
- `T` — nominal thickness (can be overridden per-element by `THICK` group)
- `Nip` — number of integration points through thickness (default 5 for TYPE1)
- Hourglass control type and coefficients
- Shear correction factor κ (for Reissner-Mindlin elements)
- Drilling DOF penalty (for TYPE11)

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `engine/source/elements/README.md` — shell element formulations using these properties
