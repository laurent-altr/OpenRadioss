# Initial Velocity (`engine/source/loads/general/inivel/`)

Applies `/INIVEL` prescribed initial velocity conditions to nodes at the start of the simulation (t = 0).

## Key Files

| File | Role |
|------|------|
| `inivel_init.F90` | Read and store initial velocity data from the restart file |
| `inivel_start.F90` | Apply initial velocities to node velocity array at t = 0 |
| `inivel_dt2.F90` | Initial velocity contribution for TYPE2 time-integration start-up |

## Algorithm

Initial velocity is an impulse applied once at t = 0 before the first time step. `inivel_start.F90` directly writes into the nodal velocity array `V(node, 1:6)` (3 translational + 3 rotational DOFs). When rigid bodies are present, the velocity is projected onto the rigid-body kinematics. `inivel_dt2.F90` handles the special case where the starter half-step displacement must account for the prescribed velocity (leap-frog initialisation).

## Related Documentation

- `engine/source/loads/general/README.md` — parent directory
- `starter/source/loads/README.md` — initial velocity input in starter
