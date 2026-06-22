# TYPE11 Interface (`engine/source/interfaces/int11/`)

TYPE11 — Node-to-segment tied contact (rigid tie, no failure). The simplest tied interface for permanently bonded surfaces.

## Key Files

| File | Role |
|------|------|
| `i11ass3.F` | Assemble TYPE11 forces |
| `i11cor3.F` | Kinematic correction: enforce tied constraint |
| `i11cdcor3.F` | Tied correction with damping |

## TYPE11

TYPE11 permanently ties slave nodes to the master surface. Slave nodes are constrained to follow the master surface exactly — no failure allowed. Used for:
- Meshing incompatibilities (tied non-conforming meshes)
- Welded connections that will not fail

Unlike TYPE2 (which uses Lagrange multipliers and is more accurate), TYPE11 uses penalty. For accuracy in stress-critical applications, prefer TYPE2.

## Related Documentation

- `engine/source/interfaces/README.md` — parent directory
