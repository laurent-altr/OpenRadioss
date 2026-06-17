# Shell Offset Contact (`engine/source/interfaces/shell_offset/`)

Handles contact surfaces offset from the shell mid-plane — accounts for shell thickness in contact gap computation.

## Key Files

| File | Role |
|------|------|
| `inter_offset_dim.F90` | Compute offset dimensions for shell contact surface |
| `inter_offset_ini.F90` | Initialise offset parameters for contact interface |
| `offset_nproj.F90` | Project node onto offset contact surface (accounts for half-thickness) |

## Shell Offset Problem

Standard shell elements have nodes on the mid-surface, but physical contact occurs at the shell surface (offset by t/2). When two shells contact:
- Without offset: contact detected when mid-planes are at gap `g = 0`
- With offset: contact should be detected when surfaces are at `g = t1/2 + t2/2`

`shell_offset` corrects this by projecting slave nodes onto the offset surface (`z = t/2` from mid-plane) before computing the gap. This prevents spurious initial penetrations in models where shells are modelled with their mid-plane coordinates.

## Related Documentation

- `engine/source/interfaces/README.md` — parent interfaces directory
- `engine/source/interfaces/int07/README.md` — TYPE7 contact uses offset correction
