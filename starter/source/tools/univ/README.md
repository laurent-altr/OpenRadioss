# Universal Beam Cross-Sections (`starter/source/tools/univ/`)

Utilities for working with standard library cross-sections (I-beams, channels, tubes, etc.) used by beam elements.

## Key Files

| File | Role |
|------|------|
| `inicod.F` | Initialise standard cross-section codes (decode cross-section type from integer code) |
| `inv3.F` | 3×3 matrix inverse (used for cross-section inertia tensor) |
| `istr.F` | Compute area moments of inertia (I_yy, I_zz, I_yz) for integer-coded cross-section |
| `rstr.F` | Compute cross-section properties for real-parametrised sections |
| `stri.F` | Compute torsion constant J for a given cross-section |
| `strr.F` | Compute warping constant Γ for open thin-walled cross-sections |

## Standard Cross-Section Library

OpenRadioss supports a library of standard structural profiles (TYPE3 beam property):

| Code | Section |
|------|---------|
| 1 | Rectangular solid |
| 2 | Circular solid |
| 3 | Circular hollow tube |
| 4 | Rectangular hollow tube |
| 5 | I-section (symmetric) |
| 6 | T-section |
| 7 | L-section (angle) |
| 8 | C-channel |

`inicod.F` maps the section code to the computation routines in `istr.F` / `rstr.F` which compute area `A`, moments `I_yy`, `I_zz`, torsion `J`, and shear area factors.

## Related Documentation

- `starter/source/tools/README.md` — parent tools directory
- `starter/source/properties/beam/` — beam property definition (property TYPE1/TYPE2/TYPE10)
- `starter/source/elements/beam/README.md` — beam element initialisation
