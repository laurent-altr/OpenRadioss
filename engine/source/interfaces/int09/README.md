# TYPE9 Interface (`engine/source/interfaces/int09/`)

TYPE9 — Tied interface with failure (node-to-segment, initially tied, fails at a force/energy threshold).

## Key Files

| File | Role |
|------|------|
| `i9cor3.F` | Kinematic correction: enforce tied constraint |
| `i9avel.F` | Velocity averaging at tied interface |
| `i9frms.F` | Frame computation for tied contact |

## TYPE9

TYPE9 starts as a perfectly tied contact (no relative motion allowed) and fails (becomes TYPE7 sliding contact) when:
- Normal force exceeds `Fn_max`
- Tangential force exceeds `Ft_max`
- Energy release rate exceeds `Gc`

Used for adhesive joints, spot welds, and composite delamination where initial bonding and progressive failure are needed.

## Related Documentation

- `engine/source/interfaces/README.md` — parent directory
