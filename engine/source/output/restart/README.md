# Engine Restart Output (`engine/source/output/restart/`)

Reads and writes engine restart files (`_nnnn.rst`): the complete solver state needed to resume a simulation from a checkpoint.

## Key Files

| File | Role |
|------|------|
| `wrrest.F` | Write main restart file: nodal state, element data, material state |
| `wrrestp.F` | Write restart with parallel (SPMD) partition data |
| `rdresa.F` | Read restart file part A (model geometry and topology) |
| `rdresb.F` | Read restart file part B (state variables, history) |
| `rdcomm.F` / `wrcomm.F` | Read/write communication data for MPI domains |
| `arralloc.F` / `fillxdp.F` | Restart array allocation and fill utilities |
| `write_matparam.F` | Write material parameters to restart |
| `write_failparam.F` | Write failure criterion state to restart |
| `write_intbuf.F` | Write interface (contact) buffer to restart |
| `write_eosparam.F90` | Write EOS parameters to restart |
| `write_sensors.F` | Write sensor states to restart |
| `write_joint.F` | Write joint state to restart |
| `write_inivel.F90` | Write initial velocity state to restart |
| `restart_rbe3pen.F90` | Restart penalty RBE3 state |
| `restart_rwallpen.F90` | Restart penalty rigid wall state |
| `w_failwave.F` | Write failure wave front state to restart |

## Architecture

The restart file stores the full state vector: nodal positions, velocities, accelerations, all element integration-point history variables (plastic strain, damage, eos state), and contact buffer. `wrrest.F` is called at the `/PRINT` interval. On restart, `rdresa.F` + `rdresb.F` restore the state before the time loop resumes.

## Related Documentation

- `engine/source/output/README.md` — all output types
- `common_source/output/restart/README.md` — common restart utilities
