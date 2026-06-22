# Model Transformations (`starter/source/model/transformation/`)

Applies geometric transformations (translation, rotation) to node coordinates during model assembly.

## Key Files

| File | Role |
|------|------|
| `lectrans.F` | Read `/TRANSFORM` keyword: translation and rotation applied to a node group |
| `min_distance_grnod_to_surface.F90` | Compute minimum distance from a node group to a surface (used for proximity-based transformations) |
| `min_distance_grnod_to_xyzpos.F90` | Compute minimum distance from a node group to a 3D point |
| `transform_translate_in_local_skew.F90` | Apply translation in a local skew frame (rather than global axes) |

## Transformations

`/TRANSFORM` allows repositioning and reorienting imported mesh parts:
- **Translation**: offset all nodes in a group by a displacement vector
- **Rotation**: rotate all nodes around a specified axis/point

This is applied at the beginning of model setup, before contact detection and domain decomposition. The transformation modifies node coordinates directly — no dynamic frame tracking is needed.

## Related Documentation

- `starter/source/model/README.md` — parent model directory
- `starter/source/model/submodel/README.md` — submodel also applies transformations (`lectranssub.F`)
