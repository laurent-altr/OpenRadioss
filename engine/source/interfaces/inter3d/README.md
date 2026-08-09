# 3D General Interface Utilities (`engine/source/interfaces/inter3d/`)

General 3D contact routines shared between multiple 3D interface types.

## Key Files

| File | Role |
|------|------|
| `i12_nearest_seg.F` | Find nearest master segment to a slave node (narrow-phase) |
| `i12cor3.F` | TYPE12 3D kinematic correction |
| `i12dis3.F` | TYPE12 distance computation |
| `i12loc3.F` | TYPE12 local coordinates (project node onto segment) |
| `i12msr3.F` | TYPE12 master segment rotation |
| `i12rot3.F` | TYPE12 rotation correction |
| `i3cor3.F` | TYPE3 3D kinematic correction |
| `i3cst3.F` | TYPE3 3D constant stiffness |
| `i3dis3.F` | TYPE3 3D distance computation |
| `i3for3.F` | TYPE3 3D contact force |

## General 3D Utilities

These routines implement the narrow-phase geometry for 3D contact:
- **Projection** (`i12loc3.F`): find the closest point on a segment (bilinear patch) to a given node — solves a 2D minimization problem
- **Distance** (`i12dis3.F`, `i3dis3.F`): compute signed gap after projection
- **Force** (`i3for3.F`): penalty force normal to segment at projection point

The `i12_nearest_seg.F` routine does a final nearest-segment search within the candidate list from the broad-phase sort.

## Related Documentation

- `engine/source/interfaces/README.md` — parent interfaces directory
- `engine/source/interfaces/int07/README.md` — TYPE7 uses these narrow-phase routines
