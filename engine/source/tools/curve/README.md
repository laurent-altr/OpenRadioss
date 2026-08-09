# Engine Curve / Function Evaluation (`engine/source/tools/curve/`)

Runtime `/FUNCT` evaluation: interpolates time-history functions each step to compute load scale factors, material parameters, and sensor-driven quantities.

## Key Files

| File | Role |
|------|------|
| `finter.F` | Main function interpolation: linear interpolation of (x, y) table at x = t |
| `finter_smooth.F` | Smoothed function interpolation (moving-average smoothing) |
| `finter_mixed.F90` | Mixed interpolation for multi-column function tables |
| `lecfun.F` | Read function (used also by engine for dynamic function updates) |
| `interp.F` | General interpolation utility (Lagrange) |
| `table2d_vinterp_log.F` | 2D table log-linear interpolation |
| `table_tools.F` | Table query utilities |
| `python_register.F90` | Register Python-driven time function |
| `funct_python_update_elements.F90` | Update elements using Python-defined function |

## Algorithm

`finter.F` performs binary search + linear interpolation on the (x, y) table each step. Scale factors are evaluated as `scale = fscale_user × finter(t, funct_table)`. For Python-defined functions, `python_register.F90` calls the Python co-process to get the current value. `table2d_vinterp_log.F` handles 2D material tables (e.g., flow stress as function of strain and strain rate) using log-linear interpolation on the strain-rate axis.

## Related Documentation

- `engine/source/tools/README.md` — parent tools directory
- `starter/source/tools/curve/README.md` — function definition in starter
