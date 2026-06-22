# Quad (4-node) 2D Solid (solidez) (`engine/source/elements/solid/solidez/`)

4-node quadrilateral 2D solid element: plane strain, plane stress, and axisymmetric formulations sharing the same kinematics.

## Key Files

| File | Role |
|------|------|
| `c33stif2el.F` | 2-element 3×3 stiffness contribution |
| `cbatran3v.F` | Coordinate transformation |
| `gettransv.F` | Get transformation for 2D solid |
| `gfhour_or.F` | Hourglass forces for orthotropic 2D |
| `m1tot_stab24.F` | Stabilisation for 2D quadrilateral |
| `s10_icp.F` / `s10sigp3.F` / `s10volj.F` | Shared 10-node routines reused for 2D |

## Note

This directory supports the 2D (plane-strain/axisymmetric) variants of the hexahedral kinematics. The files here implement the Z-extension and coordinate transformation needed to map 2D elements into the 3D solver framework. See `engine/source/elements/solid_2d/` for the dedicated 2D element directory.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/solid_2d/README.md` — 2D element solver
