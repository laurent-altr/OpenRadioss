# 2D Interface (`engine/source/interfaces/inter2d/`)

Contact interfaces for 2D solid elements (plane strain/stress and axisymmetric).

## Key Files

| File | Role |
|------|------|
| `i3for2.F` | 2D TYPE3 contact force computation |
| `intvo2.F` | 2D interface void element detection |
| `invoi2.F` | 2D interface void initialisation |
| `chkstifn.F` | Check stiffness for 2D contact interface |

## 2D Contact

For 2D solid simulations, the contact surface degenerates from 2D faces to 1D edges. The contact algorithm is the same (gap detection → penalty force → friction) but operating on line segments rather than surface patches.

## Related Documentation

- `engine/source/interfaces/README.md` — parent interfaces directory
- `engine/source/elements/solid_2d/README.md` — 2D solid elements
