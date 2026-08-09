# Windshield Alter (Brokmann) Failure (`starter/source/materials/fail/windshield_alter/`)

Starter initialisation for /FAIL/ALTER (Brokmann windshield crack model): stochastic crack-initiation model for laminated glass under impact.

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_alter.F` | Parse /FAIL/ALTER card parameters |
| `fail_windshield_init.F` | Initialise windshield stochastic crack field |
| `brokmann_crack_init.F90` | Initialise Brokmann crack-seed positions using random distribution |
| `brokmann_random.F90` | Random number generator for crack-seed placement |
| `brokmann_elem_spmd_renum.F90` | Renumber crack-seed elements across MPI domains |
| `crack_depth_init.F90` | Initialise crack depth field from seed distribution |

## Description

The Brokmann windshield model treats crack initiation as a stochastic point process: crack seeds are distributed over the glass surface with a given areal density and random orientation. `brokmann_crack_init.F90` samples seed positions, `crack_depth_init.F90` initialises the sub-critical crack depth field, and `brokmann_elem_spmd_renum.F90` handles domain decomposition for parallel runs. The resulting crack-seed field drives the engine-side crack propagation.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/windshield_alter/README.md` — runtime crack propagation
