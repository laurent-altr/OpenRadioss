# sortie/

## Purpose
Visualization-adjacent terminal output: ASCII curve plotting and a progress bar. Used to give users real-time feedback and simple data visualization in the `.out` log file.

## Files

| File | Description |
|------|-------------|
| `plot_curve.F` | Subroutine `PLOT_CURVE` — generates an ASCII X-Y plot with configurable axes, symbols, and labels; writes the plot to the `.out` log file |
| `progbar_c.c` | Function `progbar_c` — writes an ASCII progress bar (percentage complete) to stdout; provides multiple Fortran calling-convention variants (`PROGBAR_C`, `progbar_c_`, `progbar_c__`) |

## Dependencies
- `plot_curve.F` uses: `modules/plot_curve_mod.F` (interface declaration)
- Used by: engine time-step loop (progress bar) and output routines (curve display)
