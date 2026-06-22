# TYPE24 Interface (`engine/source/interfaces/int24/`)

TYPE24 — Penalty-based segment-to-segment contact with edge contact, a high-performance alternative to TYPE25.

## Key Files

| File | Role |
|------|------|
| `i24_prepare.F` | Prepare TYPE24 data: update normals and bounding boxes |
| `i24_save_sub.F` | Save sub-contact data for restart |
| `i24cor3.F` | Kinematic correction |

## TYPE24 vs TYPE25

TYPE24 and TYPE25 are both segment-to-segment contact formulations. TYPE24:
- Uses penalty enforcement (TYPE25 can also use Lagrange multipliers)
- Slightly less robust for extreme large deformation but faster
- Good for general contact applications where TYPE7 is insufficient

## Related Documentation

- `engine/source/interfaces/README.md` — parent directory
- `engine/source/interfaces/int25/README.md` — TYPE25 (more robust)
