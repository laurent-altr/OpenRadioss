# fxbody/

## Purpose
Fixed flexible body velocity initialization — computes nodal velocities for a fixed body from master-node translational and rotational motion using rotation matrices.

## Files

| File | Description |
|------|-------------|
| `fxbvini.F` | Subroutine `FXBVINI` — initializes fixed flexible body velocities (`FXBVIT`) from master-node rotation parameters (`FXBIPM`, `FXBRPM`) and global velocity arrays (`V`, `VR`) |

## Dependencies
- Used by: flexible body initialization routines in the engine
