# interf/

## Purpose
Contact/interface geometry computation and buffer management utilities shared across all interface types. Provides basis-vector construction, buffer initialization and copy, moment equilibrium solving, and geometry projection routines.

## Files

| File | Description |
|------|-------------|
| `i2rep.F` | Subroutine `I2REP` — computes two tangent edge vectors (E1, E2) and normal vector (E3) for a quadrilateral contact patch from four node coordinates |
| `intbuf_ini.F` | Subroutine `INTBUF_INI` — allocates and initializes interface buffer structures (`INTBUF_STRUCT_`) from restart data or fresh input; reads all buffer sizes |
| `int18_law151_nsv_shift.F` | Module `int18_law151_nsv_shift_mod` — shifts secondary node ID indices at the boundaries of interaction computations for LAW151 interfaces (collocated scheme) |
| `i2loceq.F` | Subroutine `I2LOCEQ` — solves moment equilibrium equations in local contact coordinates; distributes forces across patch nodes to satisfy moment balance |
| `copy_intbuf_tab.F` | Subroutine `COPY_INTBUF_TAB` — deep-copies interface buffer structures, re-allocating MULTIMP (multi-material point) arrays that may change size |
| `nearest_seg.F` | Subroutine `NEAREST_SEG` — finds barycentric coordinates of a node projected onto a triangle patch; handles cases where the projection lies outside the triangle by returning the nearest edge |
| `i2cin_rot27.F` | Subroutine `I2CIN_ROT27` — computes contact geometry for type-27 (thin-shell/cylindrical) interfaces: surface derivatives `DPARA`, derivative `DWDU`, and rotation angles `BETAX`/`BETAY` |
| `i2pen_rot.F` | Subroutine `I2PEN_ROT` — computes rigid-body velocity and penalty contact force for type-25 (penalty method) penetration interfaces using a rotation matrix `SKEW(9)` |
| `upgrade_multimp.F` | Subroutine `upgrade_multimp` — reallocates interface buffer arrays when MULTIMP contact count changes between increments |

## Dependencies
- Uses: `modules/interfaces/intbufdef_mod.F90` (buffer type definitions), `modules/interfaces/interfaces_mod.F90` (master interface types)
- Used by: engine interface processing routines (`source/interfaces/`)
