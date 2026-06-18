# Computation Control Parameters (`starter/source/general_controls/computation/`)

Reads global computation control cards that set the analysis type, solver options, and numerical parameters.

## Key Files

| File | Role |
|------|------|
| `hm_read_analy.F` | Parse /ANALY: analysis type (explicit/implicit), termination time, energy options |
| `hm_read_caa.F` | Parse /CAA: computation acoustics analysis parameters |
| `hm_read_eig.F` | Parse /EIG: eigenvalue analysis (Lanczos) parameters |
| `hm_read_implicit.F` | Parse /IMPL: implicit solver parameters (Newton tolerance, BFGS, line search) |
| `hm_read_perturb.F` | Parse /PERTURB: geometry/material perturbation for buckling |
| `hm_read_perturb_fail.F` | Perturbation parameters for failure criteria |
| `hm_read_perturb_part_shell.F` | Shell-part perturbation parameters |
| `hm_read_perturb_part_solid.F` | Solid-part perturbation parameters |
| `hm_read_rand.F` | Parse /RANDOM: random perturbation seed and magnitude |
| `hm_read_sms.F` | Parse /AMS (Selective Mass Scaling): scaling ratio, target DT |
| `hm_read_sphglo.F` | Parse /SPH/GENERAL: global SPH kernel and neighbour parameters |
| `hm_read_spmd.F` | Parse /SPMD: MPI domain decomposition parameters |
| `hm_read_unit.F` | Parse /UNIT: unit system declaration |
| `init_random.F` | Initialise random number generator with seed |
| `plot_distrib.F` | Write domain decomposition plot data |
| `unit_code.F` | Unit system conversion factors |

## Related Documentation

- `starter/source/general_controls/README.md` — parent directory
- `starter/source/general_controls/damping/README.md` — damping control
- `engine/source/implicit/README.md` — implicit solver implementation
- `engine/source/ams/README.md` — AMS implementation
