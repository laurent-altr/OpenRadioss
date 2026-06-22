# Engine Universal Joint (`engine/source/tools/univ/`)

Implements universal joint (Cardan joint) kinematics for beam/spring element assemblies.

## Key Files

| File | Role |
|------|------|
| `butterworth.F` | Butterworth filter (shared utility, also used by accele/) |
| `inicod.F` | Initialise codirectional constraint |
| `istr.F` | Integer-to-string conversion |
| `rstr.F` | Real-to-string conversion |
| `stri.F` | String-to-integer conversion |

## Description

The `univ/` directory contains general utility routines shared across the engine:
- `butterworth.F` is the shared low-pass filter implementation used by `accele/` and other modules
- `inicod.F` initialises the codirectional (co-aligned axis) constraint needed for universal joints and cross-beam joint models
- The `istr/rstr/stri` routines provide Fortran string conversion utilities needed for output formatting

## Related Documentation

- `engine/source/tools/README.md` — parent tools directory
- `engine/source/tools/accele/README.md` — Butterworth filter consumer
