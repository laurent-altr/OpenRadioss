# Starter LAW51 — JWL with ignition and growth (I&G) burn model Reader (`starter/source/materials/mat/mat051/`)

Reads `/MAT/LAW51` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `fill_buffer_51.F` | Starter input reader / initialiser |
| `fill_buffer_51_0.F` | Starter input reader / initialiser |
| `hm_read_mat51.F` | Starter input reader / initialiser |
| `hm_read_mat51_iform11.F` | Starter input reader / initialiser |
| `ie_bound.F` | Starter input reader / initialiser |
| `lecm51__check_initial_state.F` | Starter input reader / initialiser |
| `m51init.F` | Starter input reader / initialiser |
| `mat51_associate_eos.F90` | Starter input reader / initialiser |
| `nrf51ini.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat051/README.md` — corresponding engine law
