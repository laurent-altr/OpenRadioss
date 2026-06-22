# TYPE18 Interface (`engine/source/interfaces/int18/`)

TYPE18 — Tied interface with spot weld failure (node-to-segment, tied until spot weld criterion).

## Key Files

| File | Role |
|------|------|
| `i18dst3.F` | Distance computation for TYPE18 |
| `i18for3.F` | Contact force with spot weld law |
| `i18main_kine.F` | Main kinematic loop for TYPE18 |

## TYPE18

TYPE18 is designed for spot weld simulation:
- Initially tied (no relative motion)
- Failure criterion based on combined normal+shear force: `(Fn/Fn0)^m + (Ft/Ft0)^n > 1`
- After failure, node is released (no further contact)

Used for resistance spot welds (RSW) in automotive crash simulations.

## Related Documentation

- `engine/source/interfaces/README.md` — parent directory
