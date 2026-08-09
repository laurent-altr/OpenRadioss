# Engine SKEW Frame Update (`engine/source/tools/skew/`)

Updates moving SKEW/FRAME reference frames each time step for follower-force and follower-constraint applications.

## Key Files

| File | Role |
|------|------|
| `movfram.F` | Update SKEW/MOV frame: rotate frame with node(s) |
| `newskw.F` | Compute new skew orientation from current node positions |
| `relfram.F` | Relative frame: frame defined by two nodes |
| `relfram_m1.F` | Relative frame TYPE1 update |
| `rotbmr.F` | Rotate frame by rigid body motion |

## Description

`/SKEW/MOV` frames follow node motion each step. `movfram.F` recomputes the 3×3 rotation matrix of the moving frame from the current positions of the defining nodes. `relfram.F` handles frames defined relative to two nodes (direction vector from node A to node B). The updated frame is then used by follower forces, follower BCS, and sensor evaluations in the local frame.

## Related Documentation

- `engine/source/tools/README.md` — parent tools directory
- `starter/source/tools/skew/README.md` — SKEW/FRAME definition in starter
