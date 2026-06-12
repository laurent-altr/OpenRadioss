# tools/clipping/

## Purpose
2D polygon clipping operations using the Weiler-Atherton algorithm. Used for computing intersections of contact segments and ALE cell faces.

## Files

| File | Module | Description |
|------|--------|-------------|
| `polygon_mod.F90` | `POLYGON_MOD` | Core 2D polygon types: `polygon_point_` (x, y coordinates), `polygon_` (polygon with coordinate array), `polygon_list_` (collection of polygons); subroutine `polygon_addpoint` |
| `polygon_clipping_mod.F90` | `POLYGON_CLIPPING_MOD` | Weiler-Atherton polygon clipping implementation; function `intersectPt` computes the intersection point of two line segments; uses types from `polygon_mod` |

## Key Types Exported
- **`polygon_point_`** — 2D point
- **`polygon_`** — polygon with dynamic coordinate array
- **`polygon_list_`** — list of polygons

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: ALE/FVM cell intersection routines, contact segment clipping
