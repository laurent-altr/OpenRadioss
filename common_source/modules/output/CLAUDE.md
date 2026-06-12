# modules/output/

## Purpose
Output data structure definitions for animation files, state files, time history records, and checksums. These types configure and carry output data from the engine computation core to the output writers.

## Files

| File | Module | Description |
|------|--------|-------------|
| `output_mod.F90` | `OUTPUT_MOD` | Main output configuration and control structures; high fan-out module used by all output-generating routines (animation, restart, DynaI) |
| `anim_mod.F90` | `ANIM_MOD` | Animation output data structures |
| `state_file_mod.F90` | `STATE_FILE_MOD` | Type `state_`: SPH state file management data; includes init subroutine |
| `time_history_mod.F` | `TH_MOD` | Type `TH_`: buffer arrays and group arrays for time history (`/TH/`) output data |
| `checksum_mod.F90` | `CHECKSUM_MOD` | Type `checksum_option_`: input file checksum storage, report flags, and C interface bindings for MD5 checksum routines |

## Key Types Exported
- **`OUTPUT_MOD`** types — output control and configuration (high fan-out)
- **`TH_`** — time history buffer
- **`state_`** — SPH state file data
- **`checksum_option_`** — checksum configuration and results

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: engine output routines (`source/output/`, animation writers, restart writers)
