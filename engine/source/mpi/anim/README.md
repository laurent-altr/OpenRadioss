# MPI Animation Exchange (`engine/source/mpi/anim/`)

Gathers ply/layer animation data from all MPI domains for composite shell output.

## Key Files

| File | Role |
|------|------|
| `spmd_anim_ply_init.F` | Initialise ply animation communication pattern |
| `spmd_anim_ply_velvec.F` | Gather ply velocity vectors for animation output |
| `spmd_anim_ply_xyznod.F` | Gather ply nodal coordinates for animation |

## Description

Composite shell animation output requires per-ply stress and position data. Since shells are distributed across MPI domains, `spmd_anim_ply_*` routines gather ply data to rank 0 before writing animation frames. Called once per animation output step.

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/output/anim/README.md` — animation output
