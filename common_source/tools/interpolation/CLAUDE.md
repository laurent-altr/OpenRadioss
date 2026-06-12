# tools/interpolation/

## Purpose
Catmull-Rom spline construction, evaluation, and point projection. Used for geometry path following (sliding contact, curve-based loads) and smoothing of tabular data.

## Files

| File | Description |
|------|-------------|
| `cr_spline_knots.F` | Subroutine `cr_spline_knots` — computes the knot vector for a Catmull-Rom spline with configurable alpha: uniform (α=0), centripetal (α=0.5), or chordal (α=1.0) |
| `cr_spline_interpol.F` | Subroutine `cr_spline_interpol` — evaluates spline position and 1st/2nd order derivatives at parameter t |
| `cr_spline_length.F` | Subroutine `cr_spline_length` — computes arc length using Simpson's 3rd-order numerical integration |
| `cr_spline_point_proj.F` | Subroutine `cr_spline_point_proj` — projects a 3D point onto the spline curve via Newton-Raphson iteration; returns the projection point coordinates and distance |

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: engine contact sliding routines and geometry-following load applications
