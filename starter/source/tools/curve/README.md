# Function Curves (`starter/source/tools/curve/`)

Reads and validates `/FUNCT` (function/curve) and `/TABLE` keywords. Functions define time-dependent loading, material hardening curves, and sensor thresholds.

## Key Files

| File | Role |
|------|------|
| `lecfun.F` | Main `/FUNCT` reader: read curve ID, title, (X, Y) data pairs |
| `nbfunc.F` | Count function entries in the input deck (pre-pass) |
| `finter.F` | Piecewise-linear interpolation at a query point (used by material/load evaluation) |
| `func_inters.F` | Compute intersection of two function curves (for sensor trigger logic) |
| `func_maxy.F` | Find the maximum Y value of a function curve |
| `func_slope.F` | Compute local slope (dY/dX) at a point |
| `check_function.F` | Validate function data: check for non-monotonic X, NaN/Inf values |
| `vw_smooth.F` | Viervoll-Wegner smoothing of noisy curve data |
| `table_tools.F` | Utility routines for table data (2D tables) |
| `table2d_intersect.F` | 2D table intersection computation |
| `simple_checksum.cpp` / `simple_checksum_mod.F90` | Checksum of function curve data for restart consistency check |
| `hm_read_funct.F` | HM binary reader for `/FUNCT` |
| `hm_read_func2d.F` | HM binary reader for 2D function tables |
| `hm_read_funct_python.F90` | HM reader path for Python-defined functions |
| `hm_read_move_funct.F` | HM reader for moving reference frame functions |
| `hm_read_table.F` | HM reader for `/TABLE` keyword |
| `hm_read_table1_0.F` / `hm_read_table1_1.F` | HM reader for 1D table variants |
| `hm_read_table2_0.F` / `hm_read_table2_1.F` | HM reader for 2D table variants |

## Function Types

| Keyword | Type | Usage |
|---------|------|-------|
| `/FUNCT` | 1D piecewise-linear | Material hardening, load scale, sensor trigger |
| `/TABLE/1` | 1D table (same as `/FUNCT`) | Explicit table form |
| `/TABLE/2` | 2D table (X, Y, Z) | Strain-rate dependent material properties |

## Validation

`check_function.F` is called after reading every function curve. It warns if:
- X values are not strictly monotonic increasing (required for interpolation)
- Y values contain NaN or infinity
- The curve has fewer than 2 points

## Related Documentation

- `engine/source/tools/README.md` — engine-side `finter.F` (same routine, shared)
- `starter/source/tools/README.md` — parent tools directory
