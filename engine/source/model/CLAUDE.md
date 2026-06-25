# engine/source/model/

## Purpose
Adaptive mesh refinement (AMR / `ADMESH`) and mesh connectivity utilities.
The remeshing routines refine the mesh dynamically during the simulation,
splitting elements based on stress/strain criteria.

## Files in root `model/`

| File | Role |
|------|------|
| `admdiv.F` | Adaptive mesh division: splits elements according to refinement criterion |
| `admerr.F` | Error estimator for adaptive refinement |
| `admfor0.F` | Force transfer to refined mesh nodes |
| `admgvid.F` | Void element detection for adaptive mesh |
| `admini.F` | Adaptive mesh initialization |
| `admmap3.F` | Maps state variables (stress, strain) from old to new mesh (3-node) |
| `admmap4.F` | Maps state variables for 4-node elements |
| `admnorm.F` | Normal computation on refined mesh |
| `admordr.F` | Orders refined elements |
| `admregul.F` | Mesh regularization (smoothing) after refinement |
| `admthke.F` | Shell thickness update after refinement |
| `admvit.F` | Velocity transfer to refined mesh nodes |
| `cndint.F` | Condensed interface for refined mesh |
| `cndordr.F` | Orders condensed interface nodes |
| `rm_imp0.F` | Remesh initialization for implicit solver |

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `remesh/` | Higher-level remesh drivers and helpers |

## Dependencies
- Called by: `RESOL` (adaptive refinement trigger section) when `/ADMESH` is active
- Uses: `ELBUF_TAB` for state-variable mapping; `NODES` for coordinate/velocity transfer
