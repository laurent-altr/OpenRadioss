# Common Sortie (Diagnostic Output) Utilities (common_source/sortie)

This directory provides shared diagnostic and progress-bar output utilities used by both the starter and engine.

## Files

| File | Role |
|------|------|
| `plot_curve.F` | Write a simple ASCII plot of a load curve or function to the `.out` file |
| `progbar_c.c` | C implementation of a console progress bar (for terminal output) |

## Load Curve Plot (`plot_curve.F`)

When `/FUNCT` curves are defined, `plot_curve.F` writes a scaled ASCII representation of the curve to the `.out` file for visual verification:

```
 /FUNCT/1
 Time     |....*..........
 0.000    |*
 0.001    |  *
 0.002    |     *
```

This is generated at model validation time (in the starter) so the user can confirm the curve shape without external post-processing.

## Progress Bar (`progbar_c.c`)

Provides a terminal progress bar that updates in-place using carriage return (`\r`). Used during long preprocessing steps (mesh generation, METIS partitioning) to indicate progress.

```
Partitioning mesh: [=========>         ] 47%
```

On non-interactive terminals (batch jobs, redirected output), the progress bar is suppressed.

## "Sortie" Naming

"Sortie" is French for "output" — this reflects the historical origins of OpenRadioss (originally developed in France by Altair/ESI). The name is preserved for continuity.

## Related Documentation

- `common_source/output/README.md` — main `.out` file writing
- `engine/source/output/README.md` — engine result output (H3D, TH, etc.)
