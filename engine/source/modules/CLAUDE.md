# engine/source/modules/

## Purpose
Engine-specific Fortran modules that are not part of `common_source/`. These
provide data types and interfaces used only by the engine (not the Starter).

## Files

| File | Module | Description |
|------|--------|-------------|
| `ams_work_mod.F90` | `AMS_WORK_MOD` | Working arrays for the AMS (Advanced Mass Scaling) solver: temporary vectors used during the PCG iteration |
| `dt_mod.F` | `DT_MOD` | Time-step data module: packages current and next time-step values for thread-safe passing |
| `file_descriptor_mod.F90` | `FILE_DESCRIPTOR_MOD` | Engine file unit management: tracks open Fortran unit numbers for output files |
| `user_interface_mod.F90` | `USER_INTERFACE_MOD` | User-element and user-material interface module: type definitions for the `UTABLE` callback mechanism |

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `interfaces/` | `sh_offset_mod.F90` — `SH_OFFSET_MOD`: shell offset data type for offset shell elements (`/PROP/SHELL` with offset) |

## Dependencies
- Used by: `engine/source/ams/`, `engine/source/user_interface/`, and `RESOL`
- Not shared with Starter — engine-side only
