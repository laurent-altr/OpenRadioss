# Material Law Implementations (`engine/source/materials/mat/`)

Per-law subdirectories containing the stress update routines for each material model (constitutive integration). One subdirectory per law number (`mat001`–`mat190`).

## Law Numbering

Each `matNNN/` directory contains the engine-side constitutive routines for `/MAT/LAWNN`. The main file naming convention is `mNNlaw.F` (stress update), `mNNlawp.F` (plain stress / shell variant), `mNNlawt.F` (thermal variant), `sigepsNNg.F` (full stress-strain update for solid elements).

## Representative Laws

| Directory | Law | Description |
|-----------|-----|-------------|
| `mat001/` | LAW001 | Isotropic elastic (`/MAT/ELAST`) |
| `mat002/` | LAW002 | Elastic-plastic with isotropic hardening (`/MAT/PLAS_JOHNS`) |
| `mat003/` | LAW003 | Viscoelastic (`/MAT/VISC_PRONY`) |
| `mat004/` | LAW004 | Elastic-plastic (Cowper-Symonds strain rate) |
| `mat005/` | LAW005 | Void/null material |
| `mat006/` | LAW006 | Drucker-Prager (geomechanics) |
| `mat010/` | LAW010 | Isotropic elastic with EOS |
| `mat011/` | LAW011 | Bilinear elastic-plastic (Bauschinger effect) |
| `mat025/` | LAW025 | Composite orthotropic (`/MAT/COMPSO`) |
| `mat034/` | LAW034 | Fabric / woven composite |
| `mat036/` | LAW036 | Elastic-plastic foam |
| `mat042/` | LAW042 | Ogden hyperelastic |
| `mat058/` | LAW058 | Laminated composite shell |
| `mat069/` | LAW069 | Rubber-like (Arruda-Boyce) |
| `mat131/` | LAW131 | Advanced elasto-viscoplastic (Chaboche, multi-surface) |
| `mat163/` | LAW163 | Crushable foam |
| `mat190/` | LAW190 | User material (calls `UMAT` Fortran subroutine) |

## Architecture

Each law directory contains:
- `mNNlaw.F` — main explicit stress update (solid element 3D stress)
- `mNNlawp.F` — plane-stress variant (shell elements)
- `mNNlawt.F` — thermal coupling variant
- `mNNlaw8.F` — 8-point solid element variant
- `sigepsNNg.F` / `sigepsNNc.F` — integration-point stress update called from element loops

All laws are called through the dispatch table in `engine/source/materials/mat_share/` and receive the standard argument list: stress tensor, strain increment, history variables array, material parameter array.

## Related Documentation

- `engine/source/materials/README.md` — parent materials directory
- `engine/source/materials/mat_share/README.md` — dispatch and shared routines
- `starter/source/materials/mat/README.md` — corresponding starter input readers
