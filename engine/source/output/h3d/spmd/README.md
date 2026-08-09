# H3D SPMD Gather (`engine/source/output/h3d/spmd/`)

MPI gather routines that collect distributed result data from all domains onto the rank-0 process for H3D output.

## Key Files

| File | Role |
|------|------|
| `h3d_gather_id_val.F90` | Gather element ID–value pairs from all domains |
| `spmd_h3d_gather_i.F` | Gather integer result arrays across domains |
| `spmd_h3d_gather_i_node.F` | Gather integer nodal arrays across domains |
| `spmd_h3d_gather_i_node_part.F` | Gather integer nodal arrays by part |
| `spmd_h3d_gather_r.F` | Gather real result arrays across domains |
| `spmd_h3d_gather_r_nodal_value.F` | Gather real nodal values across domains |
| `spmd_h3d_gather_r_node.F` | Gather real nodal arrays across domains |
| `spmd_h3d_gather_t_node.F` | Gather tensor nodal arrays across domains |
| `spmd_h3d_getmsr.F` | Get message size for H3D gather |
| `spmd_h3d_getmsr_update.F` | Update message size after domain change |
| `spmd_h3d_sum_r_nodal_value.F` | Sum (reduce) real nodal values across domains |
| `spmd_h3d_oned_off.F` | Compute 1D element offset for gather |
| `spmd_h3d_quad_off.F` | Compute 2D quad element offset for gather |
| `spmd_h3d_shell_off.F` | Compute shell element offset for gather |
| `spmd_h3d_solid_off.F` | Compute solid element offset for gather |
| `spmd_h3d_sph_off.F` | Compute SPH particle offset for gather |

## Description

In parallel runs, each MPI domain holds only its local elements and nodes. Before writing H3D output, `spmd_h3d_gather_r.F` and related routines use `MPI_Gatherv` to collect local result arrays on rank 0, reordering by global element/node ID so the H3D file contains a complete, correctly-ordered dataset. The `*_off.F` routines compute the displacement and count arrays needed for `MPI_Gatherv`.

## Related Documentation

- `engine/source/output/h3d/README.md` — parent H3D directory
- `engine/source/mpi/README.md` — MPI communication infrastructure
