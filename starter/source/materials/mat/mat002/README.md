# Starter LAW2 — Johnson-Cook elastic-plastic with strain-rate and thermal softening (`starter/source/materials/mat/mat002/`)

Reads `/MAT/LAW2` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat02_jc.F90` | `HM_READ_MAT02_JC` — reads `/MAT/LAW2` jc variant |
| `hm_read_mat02_predef.F90` | `HM_READ_MAT02_PREDEF` — reads `/MAT/LAW2` predef variant |
| `hm_read_mat02_zerilli.F90` | `HM_READ_MAT02_ZERILLI` — reads `/MAT/LAW2` zerilli variant |
| `law02_upd.F90` | `LAW02_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat002/README.md` — corresponding engine law
