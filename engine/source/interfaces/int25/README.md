# TYPE25 Contact Interface (`engine/source/interfaces/int25/`)

TYPE25 is the most general contact interface in OpenRadioss — a segment-to-segment contact handling all element types with robust large-deformation treatment.

## Key Files

| File | Role |
|------|------|
| `i25_prepare.F` | Prepare TYPE25 data for current step: update segment normals and bounding boxes |
| `i25ass3.F` | Assemble TYPE25 contact forces to global force vector |
| `i25ass_e2s.F` | Edge-to-segment force assembly |
| `i25asse.F` | Element-side force assembly |
| `i25comp_1.F` | TYPE25 computation stage 1: detect penetration |
| `i25comp_2.F` | TYPE25 computation stage 2: compute contact force |
| `i25cor3.F` | Kinematic correction for TYPE25 (remove penetration) |
| `i25cor3_e2s.F` | Edge-to-segment kinematic correction |
| `i25cor3e.F` | Element-side correction |
| `i25dst3_1.F` | Distance computation stage 1 |

## TYPE25 vs TYPE7

| Feature | TYPE7 (node-to-surface) | TYPE25 (segment-to-segment) |
|---------|------------------------|----------------------------|
| Slave entity | Node | Surface segment |
| Master entity | Segment | Segment |
| Handles edge contact | No | Yes |
| Large deformation | Moderate | Robust |
| Self-contact | No | Yes |
| Cost | Lower | Higher |

TYPE25 is recommended for:
- Self-contact (folding, buckling into self)
- Large-deformation contact (rubber, foam)
- Edge-to-edge contact (tube crimping)
- More complex geometries

## Related Documentation

- `engine/source/interfaces/int07/README.md` — TYPE7 (simpler, cheaper)
- `engine/source/interfaces/README.md` — parent interfaces directory
