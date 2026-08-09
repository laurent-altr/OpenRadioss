# Engine `.out` Log File (`engine/source/output/outfile/`)

Writes the main run-time log file (`.out`): echoes input, reports time step history, energy/mass totals, and per-step diagnostics.

## Key Files

| File | Role |
|------|------|
| `check_nan_acc.F` | Check for NaN/Inf in accelerations and report |
| `chkmsin.F` | Check mass-scaling ratio and report if excessive |
| `collect.F` | Collect global energy/mass totals for log output |

## Contents

The `.out` file is the primary run-time log. Each time step the engine writes:
- Current simulation time and wall-clock time
- Time step `Δt` and controlling element
- Total internal energy, kinetic energy, external work
- Added mass from mass scaling (if any)
- Warnings from `check_nan_acc.F` (NaN detection) and `chkmsin.F` (mass ratio)

`collect.F` gathers these global quantities across all MPI domains before writing.

## Related Documentation

- `engine/source/output/README.md` — parent output directory
- `engine/source/output/report/README.md` — end-of-run summary
