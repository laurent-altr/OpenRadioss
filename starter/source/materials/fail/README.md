# Starter Failure Criterion Input (`starter/source/materials/fail/`)

Reads failure criterion parameters for all failure models. Each subdirectory corresponds to one criterion and contains `hm_read_fail_*.F` routines that parse the criterion's card and store parameters in the fail-parameter arrays.

## Top-Level Files

| File | Role |
|------|------|
| `hm_read_fail.F` | Master fail reader: dispatch to per-criterion subdirectory |
| `fail_init.F` | Initialise failure parameter arrays |
| `failwave_init.F` | Initialise failure wave front geometry |
| `check_pthickfail.F` | Validate per-thickness-integration-point fail settings |
| `check_swift_failure.F90` | Validate Swift criterion parameters |
| `nloc_dmg_init.F` | Initialise non-local damage regularisation |
| `nlocal_init_sta.F` | Non-local initialisation at starter level |
| `diffuse_necking_2d.F90` | Diffuse necking criterion 2D check |
| `write_failparam.F` | Write failure parameters to restart |

## Subdirectories

One subdirectory per failure criterion, each containing the `hm_read_fail_*` reader(s) for that criterion. The subdirectory names match `engine/source/materials/fail/` exactly. Additional starter-only criteria:

| Directory | Criterion |
|-----------|-----------|
| `failuser/` | User-defined failure criterion |
| `fractal/` | Fractal (multi-scale) failure |
| `gurson/` | Gurson-Tvergaard-Needleman void-growth model |
| `windshield_alter/` | Wind glass fracture (starter version of alter/) |

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `engine/source/materials/fail/README.md` — engine-side fail criterion implementations
