# Output Modules (`common_source/modules/output/`)

Fortran 90 modules defining data types for output subsystem state. Shared between starter and engine.

## Modules

| File | Module | Contents |
|------|--------|---------|
| `output_mod.F90` | `OUTPUT_MOD` | Global output state: output frequencies, flags for each output type (H3D/anim/TH/restart) |
| `anim_mod.F90` | `ANIM_MOD` | Animation output parameters: variables requested, output interval, nodal/element selection |
| `time_history_mod.F` | `TIME_HISTORY_MOD` | Time history (`/TH`) output data: sensor node lists, output variable codes |
| `state_file_mod.F90` | `STATE_FILE_MOD` | Restart file state: current restart sequence number, file handle, write interval |
| `checksum_mod.F90` | `CHECKSUM_MOD` | Checksum state: accumulated checksum value, comparison reference |

## OUTPUT_MOD

The central module controlling what gets written and when:
- `DTANIM` — animation output interval
- `DTRST` — restart file write interval
- `DTTH` — time history output interval
- `IOUTFLAG` — bitmask: which output types are active

Read by the starter from `/ANIM`, `/TH`, `/H3D`, `/PRINT` keywords. The engine reads these values at startup and triggers the appropriate output routines at each qualifying time step.

## Related Documentation

- `common_source/modules/README.md` — parent modules directory
- `engine/source/output/README.md` — output routines
- `starter/source/output/README.md` — output parameter reading
