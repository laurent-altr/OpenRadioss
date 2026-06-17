# H3D Output (`engine/source/output/h3d/`)

Writes Altair H3D binary output: nodal displacements, element stresses/strains, contact forces, and all result quantities viewable in HyperView.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `h3d_build_cpp/` | C++ H3D API wrappers: create mesh entities (nodes, quads, shells, solids, SPH, beams, RBEs, rigid walls) and result datatypes |
| `h3d_build_fortran/` | Fortran wrappers calling C++ API: `h3d_ini.F`, `lech3d.F`, `prelech3d.F90`, per-element-type create/update routines |
| `h3d_results/` | Per-timestep result output: `genh3d.F` (master), per-type scalars/tensors/vectors, FLD strain output |
| `spmd/` | MPI gather for H3D: `spmd_h3d_gather_r.F`, domain-offset routines, nodal/element value assembly across ranks |
| `input_list/` | Keyword-to-result-ID mapping: `h3d_gene_keyword.F`, `h3d_list_*.F` — maps Radioss keywords to H3D data IDs |

## Architecture

H3D is a compressed binary format used by Altair HyperWorks. The output pipeline:

1. `h3d_build_fortran/h3d_ini.F` initialises the H3D file and writes the mesh topology once per run
2. `h3d_build_cpp/` provides the C++ H3D SDK interface
3. `h3d_results/genh3d.F` is called each output interval to write the current step's results
4. `spmd/` gathers distributed results from all MPI ranks before writing

Each element type (quads, shells, solids, SPH, 1D) has dedicated scalar, vector, and tensor output routines.

## Related Documentation

- `engine/source/output/README.md` — all output types
- `engine/source/output/th/README.md` — time-history (TH) output
