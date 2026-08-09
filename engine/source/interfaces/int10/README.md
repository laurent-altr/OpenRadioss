# TYPE10 Interface (`engine/source/interfaces/int10/`)

TYPE10 — Node-to-segment contact for 2D plane-strain/axisymmetric problems.

## Key Files

| File | Role |
|------|------|
| `i10for3.F` | Contact force computation for TYPE10 |
| `i10dst3.F` | Distance computation |
| `i10corp3.F` | Kinematic correction |

## TYPE10

TYPE10 is the 2D analogue of TYPE7 — node-to-edge contact for quad/triangle 2D solid elements. Slave nodes are tested against master line segments (element edges). Used for:
- Metal forming simulations (2D cross-section)
- Rubber seal compression

## Related Documentation

- `engine/source/interfaces/README.md` — parent directory
- `engine/source/elements/solid_2d/README.md` — 2D solid elements
