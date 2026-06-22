# Interpolation Utilities (`common_source/tools/interpolation/`)

Cubic spline interpolation and parametric curve utilities, shared between starter and engine.

## Key Files

| File | Role |
|------|------|
| `cr_spline_interpol.F` | Evaluate cubic spline at a query point: returns interpolated value |
| `cr_spline_knots.F` | Compute cubic spline knot coefficients from control points |
| `cr_spline_length.F` | Compute arc length of a parametric cubic spline curve |
| `cr_spline_point_proj.F` | Project a 3D point onto a parametric spline curve (find closest point) |

## Catmull-Rom / Natural Cubic Spline

These routines implement cubic spline interpolation through a set of data points:
1. `cr_spline_knots.F` — solve the tridiagonal system for the second derivatives (once, at setup)
2. `cr_spline_interpol.F` — evaluate at any query point using the precomputed coefficients (repeated, during time integration)

The `cr_` prefix refers to Catmull-Rom, though the implementation covers standard cubic splines with natural (zero second-derivative) end conditions.

## Usage in OpenRadioss

- **Material models**: smooth interpolation of tabulated stress-strain data between data points
- **ALE/Euler**: spline approximation of free-surface interfaces
- **Field mapping**: interpolating solution fields from a coarse global mesh onto a submodel boundary

For 1D piecewise-linear function look-up (the common case for `/FUNCT` curves), use `finter.F` in `engine/source/tools/curve/` — it is faster for piecewise-linear data.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `engine/source/tools/README.md` — piecewise-linear curve interpolation (`finter.F`)
