# Animation Output (`engine/source/output/anim/`)

Writes legacy Radioss animation files (`.rnnn` format) and provides reader utilities for post-processing.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `generate/` | Animation generation: `ani_pcont.F` (contact pressure), `ani_txt.F` (text output), `anicon0/2.F` (contour data), `anim_crk_init.F` (crack animation), nodal/element contour routines |
| `reader/` | Animation reader: `freanim.F` (read animation frame), index build/reset utilities |

## Key Files

| File | Role |
|------|------|
| `generate/ani_pcont.F` | Contact pressure field for animation |
| `generate/anicon0.F` | Elemental contour animation output (stresses, strains) |
| `generate/anicon2.F` | Extended elemental contour (alternate formulation) |
| `generate/anim_nodal_contour_fvmbags.F` | Nodal contour for FV airbag nodes |
| `reader/freanim.F` | Read animation frame data for conversion/restart |

## Notes

Animation files are the legacy OpenRadioss output format (pre-H3D). They remain supported for backward compatibility. For new work H3D (`engine/source/output/h3d/`) is the preferred output format.

## Related Documentation

- `engine/source/output/README.md` — all output types
- `engine/source/output/h3d/README.md` — modern H3D format
