# STA (State) Output (`engine/source/output/sta/`)

Writes binary state files (`.nnnn` animation frames) in the legacy Radioss state format for elemental and nodal results.

## Key Files

| File | Role |
|------|------|
| `genstat.F` | Master state writer: gather and write one animation frame |
| `stat_shel_mp.F` / `stat_shel_spmd.F` | Shell element state (MP/SPMD variants) |
| `stat_brick_mp.F` / `stat_brick_spmd.F` | Solid element state (MP/SPMD variants) |
| `stat_beam_mp.F` / `stat_beam_spmd.F` | Beam element state |
| `stat_quad_mp.F` / `stat_quad_spmd.F` | Quad (2D) element state |
| `stat_spring_mp.F` / `stat_spring_spmd.F` | Spring element state |
| `stat_truss_mp.F` / `stat_truss_spmd.F` | Truss element state |
| `stat_sphcel_full.F90` / `stat_sphcel_mp.F90` / `stat_sphcel_spmd.F90` | SPH cell state |
| `stat_node.F` | Nodal displacement, velocity, acceleration state |
| `stat_c_straf.F` / `stat_c_strsf.F` | Solid strain/stress field state |
| `stat_s_straf.F` / `stat_s_strsf.F` | Shell strain/stress field state |
| `stat_c_fail.F` / `stat_s_fail.F` | Failure indicator state |
| `stat_n_bcs.F` | BCS (boundary condition) nodal state |
| `sta_c_get_q4lsys.F` / `sta_c_get_t3lsys.F` | Local coordinate system for quads/triangles |
| `state_n_vel.F` | Nodal velocity state |

## Related Documentation

- `engine/source/output/README.md` — all output types
- `engine/source/output/anim/README.md` — animation output (uses STA frames)
