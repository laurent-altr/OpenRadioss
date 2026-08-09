# Starter LAW51 — JWL with ignition and growth (I&G) burn model (`starter/source/materials/mat/mat051/`)

Reads `/MAT/LAW51` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `fill_buffer_51.F` | `FILL_BUFFER_51` — starter helper routine |
| `fill_buffer_51_0.F` | `FILL_BUFFER_51_0` — starter helper routine |
| `hm_read_mat51.F` | `HM_READ_MAT51` — reads `/MAT/LAW51` keyword parameters into PM/IPM |
| `hm_read_mat51_iform11.F` | `HM_READ_MAT51_IFORM11` — reads `/MAT/LAW51` iform11 variant |
| `ie_bound.F` | Starter helper |
| `lecm51__check_initial_state.F` | `LECM51__CHECK_INITIAL_STATE` — initialises PM array entries and state variables |
| `m51init.F` | `M51INIT` — initialises PM array entries and state variables |
| `mat51_associate_eos.F90` | `MAT51_ASSOCIATE_EOS` — starter helper routine |
| `nrf51ini.F` | `NRF51INI` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat051/README.md` — corresponding engine law
