# engine/source/system/

## Purpose
Low-level system utilities and support routines: error handling, timing,
machine-specific operations, process control, Parith/ON skyline allocation,
and sorting helpers. These are not FEA physics — they are infrastructure.

## Key files

| File | Role |
|------|------|
| `arret.F`, `arret_message.F` | Fatal error handler: prints message and calls `MPI_ABORT` / `EXIT` |
| `error_alloc.F` | Allocation error handler: prints descriptor and aborts |
| `iniconstant.F` | Initializes physical constants (calls `CONSTANT_MOD`) |
| `inicod.F` (in `tools/univ/`) | Code flag initialization: sets integer flags from keyword options |
| `machine.F` | Machine-specific floating-point flags (`HUGE`, `TINY`, `EPSILON`) |
| `my_isnan.F` | NaN detection (`IEEE_IS_NAN` wrapper) |
| `parit.F` | Parith/ON setup: allocates skyline arrays (`FSKY`, `FSKYV`, `FSKYM`) and builds skyline address tables |
| `reallocate_skyline.F` | Grows skyline array if model size exceeds initial estimate |
| `sort_mid_pid.F` | Sorts element lists by material/property ID |
| `sysfus.F` | Fusion: merges data structures after domain decomposition |
| `timer.F`, `timer_c.c`, `timer_interf.F`, `timer_mod.F90` | Wall-clock timer: `TIMINC` (start), `TIMEND` (stop) — used throughout `RESOL` for performance profiling |
| `zero.F` | Fast array zeroing using vectorized loops |
| `aleat.F` | Pseudo-random number generator |
| `get_file_name_info.F` | Parses file names and extensions |
| `open_tempfile.F` | Opens a scratch temporary file safely |
| `printime.F` | Prints timing summary to `.sta` stats file |
| `stacksize_c.c` | Sets the OS stack size limit |
| `tmpenv_c.c` | Reads temporary directory path from environment |
| `trace_back.F`, `traceback_handler.c` | Signal handler that prints a stack trace on crash |
| `progcond_c.c`, `mon_c.c` | Progress monitoring: writes `.prog` status file |
| `pid.c` | Process ID query |

## Dependencies
- Called by: virtually all engine routines (`ARRET` for errors, timers in `RESOL`)
- No external FEA dependencies — purely infrastructure
