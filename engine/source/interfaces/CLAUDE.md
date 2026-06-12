# engine/source/interfaces/

## Purpose
Contact and interface mechanics: geometric proximity search (voxel-based),
penetration computation, contact force assembly, and friction models. Called
from `RESOL` at Step 4 (lines ~3650–3960) each cycle.

## Architecture

The main entry points called from `RESOL` are:

| Routine | File | Dispatches |
|---------|------|-----------|
| `INTFOP8` | `interf/intfop8.F` | TYPE 8 — rigid wall (plane/sphere/cylinder/cone) |
| `INTFOP1` | `interf/intfop1.F` | TYPE 1 — tied interface (node-on-surface) |
| `INTFOP2` | `interf/intfop2.F` | Types 2, 5, 7, 9, 10, 11, 14, 15, 16, 17, 18, 20–25 — all other contact |

`INTFOP2` loops over all active interface groups and dispatches by type via
a SELECT CASE on the interface type index (stored in the interface descriptor).

## Sub-directories

### `interf/` — Main dispatchers and shared geometry checks
- `intfop1.F`, `intfop2.F`, `intfop8.F` — the 3 main dispatchers
- `chkstfn3.F` — contact stiffness check
- `check_*_state.F`, `find_*_inter.F` — edge/surface state checking and detection
- `count_nb_elem_edge.F` — counts elements per contact edge
- `dealloc_shoot_inter.F` — deallocates shooting search buffers

### `intsort/` — Proximity search (voxel bucket sort)
- `intcrit.F` — interface search criteria: buckets candidate slave–master pairs
- `inttri.F` — triangle/quad segment proximity test
- `intbox.F`, `intvox.F` — voxel box search for candidate pairs
- `collision_mod.F` — C++ collision detection module

### `generic/` — General contact utilities
- `inter_init_component.F90`, `inter_init_component_list.F90` — interface component initialization
- `inter_box_creation.F` — creates bounding box for contact segments
- `inter_color_voxel.F`, `inter_color_coarse_voxel.F` — voxel coloring for parallel contact search
- `inter_curv_computation.F` — curvature computation for smooth contact
- `inter_prepare_sort.F` — prepares candidate list for sorting

### `int07/` — TYPE 7 (node-to-surface, penalty, most common)
- `i7for3.F` — force computation
- `i7cor3.F`, `i7cor3t.F`, `i7cork3.F` — penetration correction
- `i7dst3.F`, `i7dstk3.F` — distance computation
- `i7ass3.F` — force assembly
- `i7cdcor3.F` — friction correction
- `i7curv.F` — curvature-based contact
- `frictionparts_model.F` — friction parts model

### `int09/` — TYPE 9 (tied BCS)
### `int10/` — TYPE 10 (tied shell edges)
### `int11/` — TYPE 11 (edge-to-edge)
- `i11for3.F`, `i11cor3.F`, `i11dst3.F` — force, correction, distance
- `i11mainf.F` — main force loop
- `i11buce.F`, `i11buce_crit.F` — bucket search variants
- `i11ke3.F`, `i11keg3.F` — kinematic energy update
- `i11cdcor3.F`, `i11corp3.F`, `i11ass3.F` — correction, projection, assembly

### `int14/` — TYPE 14 (self-contact / automatic)
### `int15/` — TYPE 15 (surface-to-surface, like type 7 but symmetric)
### `int16/` — TYPE 16 (contact + friction, smooth penalty)
### `int17/` — TYPE 17 (contact with adhesion)
### `int18/` — TYPE 18 (fluid-structure contact for ALE)
### `int20/`, `int21/` — TYPE 20/21 (rigid-body contact)
### `int22/`, `int23/` — TYPE 22/23 (specialized)
### `int24/` — TYPE 24 (mortar contact, accurate large-sliding)
- `i24for3.F`, `i24cor3.F`, `i24dst3.F` — mortar force, correction, distance
- `i24_prepare.F` — mortar segment setup
- `i24gap_pxfem.F` — XFEM gap for mortar
- `i24intarea_fic.F90` — fictitious integration area

### `int25/` — TYPE 25 (edge-to-edge mortar)
- `i25for3.F`, `i25cor3.F`, `i25dst3.F` — force, correction, distance
- `i25ass3.F`, `i25asse.F` — assembly (node/edge)
- `i25comp_1.F`, `i25comp_2.F` — constraint computation

### `inter2d/` — 2D contact (for 2D plane-strain models)
### `inter3d/` — 3D contact helpers
### `shell_offset/` — Shell offset contact (offset mid-plane to real surface)

## Key data structures
Contact interface state is stored in `INTERFACES_MOD` structures (defined in
`common_source/modules/interfaces/`). The `ELEMENT%PON` field (for Parith/ON
skyline contact) lives in `parith_on_mod.F90`.

## MPI exchange
After `INTFOP8/1/2` force assembly:
- `SPMD_EXCH_PRESS` — pressure exchange
- `SPMD_EXCH_A_INT2` — contact force exchange (Parith/OFF)
- `SPMD_EXCH_A_INT2_PON` — contact force exchange (Parith/ON)
- Candidate exchange: `engine/source/mpi/interfaces/send_cand.F`

## Dependencies
- Called by: `RESOL` Steps 4 (lines ~3650–3960) and after (MPI exchange)
- Uses: `common_source/modules/interfaces/interfaces_mod.F90`, `NODES%A`
