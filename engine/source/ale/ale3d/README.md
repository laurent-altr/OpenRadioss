# ALE 3D (`engine/source/ale/ale3d/`)

3D ALE advection step for hexahedral elements.

## Key Files

| File | Role |
|------|------|
| `aconv3.F` | 3D ALE convection: advect density, momentum, energy |
| `adiff3.F` | 3D diffusion |
| `afimp3.F` | 3D implicit mass flux |
| `aflux3.F` | 3D explicit mass flux across faces |
| `agrad3.F` | 3D gradient reconstruction (van Leer limiter) |
| `arezo3.F` | 3D rezoning — compute grid velocity |
| `a4conv3.F` | TYPE4 ALE 3D convection (alternative scheme) |
| `a4flux3.F` | TYPE4 3D flux |
| `iqel03.F` | 3D element quality metric |
| `tgrad3.F` | 3D thermal gradient advection |

## 3D ALE vs 2D

The 3D routines handle hexahedral (8-node) elements on 3D grids. The algorithm is the same as 2D (Lagrange+rezone+advect) but with:
- 6 faces per element (vs 4 in 2D)
- Full 3D gradient reconstruction
- Higher FLOP count per element

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `engine/source/ale/ale2d/README.md` — 2D version
- `engine/source/ale/alemuscl/README.md` — MUSCL scheme used for reconstruction
