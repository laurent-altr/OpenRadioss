# MPI Kinematic Condition Exchanges (`engine/source/mpi/kinematic_conditions/`)

SPMD exchanges for rigid body, RBE2, and rigid-link kinematic constraints that span domain boundaries.

## Key Files

| File | Role |
|------|------|
| `fr_rlink1.F` | Exchange rigid-link forces across domains |
| `spmd_exch_a_rb6.F` | Exchange 6-DOF rigid body acceleration |
| `spmd_exch_a_rb6g.F` | Exchange rigid body acceleration (grouped) |
| `spmd_exch_a_rm6.F` | Exchange rigid body resultant moment |
| `spmd_exch_fr6.F` | Exchange 6-DOF rigid body force from slave nodes |

## Related Documentation

- `engine/source/mpi/README.md` — MPI overview
- `engine/source/constraints/general/rbody/README.md` — rigid body implementation
