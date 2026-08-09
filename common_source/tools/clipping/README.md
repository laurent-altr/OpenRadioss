# Polygon Clipping (`common_source/tools/clipping/`)

Sutherland-Hodgman polygon clipping algorithm, shared between starter and engine.

## Key Files

| File | Role |
|------|------|
| `polygon_clipping_mod.F90` | Fortran module implementing Sutherland-Hodgman algorithm for convex polygon clipping |
| `polygon_mod.F90` | Fortran module defining polygon data type (vertex list) and geometric utilities |

## Sutherland-Hodgman Algorithm

Clips an arbitrary polygon against each half-plane of a convex clipping region in turn. Each clip step:
1. Iterates over polygon edges
2. Tests each vertex against the current clip plane
3. Outputs vertices (possibly with an intersection point) based on inside/outside classification

The algorithm handles concave input polygons but requires a convex clipping region.

## Usage in OpenRadioss

### ALE Interface Tracking
When Lagrangian structural elements interface with an ALE/Euler fluid domain, element faces are clipped against fluid cell boundaries to compute intersection areas and volumes. This drives the coupling flux computation.

### Section Cuts
Output section cuts (`/SECT`) compute the intersection of a cut plane with element faces. Each face polygon is clipped against the cut plane's half-space to determine the contributing area.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `engine/source/ale/README.md` — ALE interface uses polygon clipping
- `engine/source/tools/README.md` — section cut implementation
