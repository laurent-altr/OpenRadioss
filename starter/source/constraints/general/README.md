# General Constraints (`starter/source/constraints/general/`)

Reads and initialises all structural constraint types: boundary conditions, rigid bodies, imposed velocity, cylindrical joints, RBE2/RBE3, rigid links, merges, MPCs, rigid walls, and general joints.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `bcs/` | Boundary conditions (fixed DOFs) |
| `rbody/` | Rigid body definitions |
| `rbe2/` | RBE2 rigid spider elements |
| `rbe3/` | RBE3 interpolation elements |
| `cyl_joint/` | Cylindrical joint constraints |
| `impvel/` | Imposed velocity boundary conditions |
| `merge/` | Node merge constraints |
| `mpc/` | Multi-point constraints |
| `rwall/` | Rigid wall obstacles |
| `gjoint/` | General kinematic joints |

## Key Files (top-level)

| File | Role |
|------|------|
| `kinchk.F` | Check kinematic constraint compatibility |
| `kinini.F` | Initialise all kinematic constraints |
| `kinset.F` | Set kinematic constraint data arrays |

## Related Documentation

- `starter/source/constraints/README.md` — parent directory
- `engine/source/constraints/README.md` — engine-side constraint enforcement
