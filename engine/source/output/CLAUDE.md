# engine/source/output/

## Purpose
All output generation: animation files, H3D results, time-history ASCII, restart
files, stats files, dynain, and message/report utilities. The main dispatcher
`SORTIE_MAIN` (`sortie_main.F`) is called from `RESOL` at Step 13 each cycle.

## Root-level files

| File | Role |
|------|------|
| `sortie_main.F` | **Main output dispatcher**: checks time triggers; calls `GENANI`/`GENSTAT`, `GENH3D`, `HIST2`, `GENDYNAIN` |
| `ecrit.F` | Low-level formatted write to output files |
| `ini_outmax.F` | Initializes max-tracking arrays (max stress, displacement, etc.) for output |
| `outmaxsubr.F` | Updates max-tracking arrays each cycle |
| `inter_nodal_areas.F` | Computes nodal areas at interfaces for output |
| `reaction_forces_th.F`, `reactions.F` | Computes reaction forces at fixed nodes |
| `sortie_error.F` | Error output helper |

## Sub-directories

### `anim/` — Animation output (`.anim` binary)
- `generate/animx.F` — main animation writer; gathers element state from `ELBUF_TAB` into animation buffer
- `generate/anicon0.F`, `anicon2.F` — animation contour variable computation
- `generate/anioff0.F`, `anioff6.F` — element off-flag animation output
- `generate/anim_nodal_*.F` — nodal quantity extraction (pressure, sound speed, FVM bags)
- `generate/animig3d.F` — 3D element animation
- `generate/genstat.F` — stats (`/PRINT`) file writer
- `reader/` — reads existing animation files (for restart from animation state)

### `h3d/` — H3D result file (Altair HyperView binary)
- `h3d_results/genh3d.F` — main H3D writer; called from `SORTIE_MAIN` for `/H3D/dt` triggers
- `h3d_build_cpp/` — C++ H3D library interface layer
- `h3d_build_fortran/` — Fortran wrappers around H3D C++ API
- `input_list/` — H3D result variable selection
- `spmd/` — MPI gather for H3D (collects per-domain results on rank 0)

### `th/` — Time-history ASCII (`.thXXXX` files)
- `hist2.F` — `HIST2`: dispatcher called from `SORTIE_MAIN`; calls element-specific TH routines
- `hist1.F`, `hist13.F` — sub-routines for specific element types
- `init_th.F`, `init_th0.F`, `init_th_group.F` — TH initialization
- `bcs1th.F` — BCS (fixed node) time history
- `thbcs.F` — temperature time history
- `th_time_output.F` — time-step for TH output
- `surf_area.F`, `surf_mass.F` — surface area/mass TH variables
- `init_reac_nod.F` — reaction node initialization for TH
- `read_th_restart.F` — reads TH state from restart

### `restart/` — Restart files (`_NNNN.rst`)
- `wrrestp.F` — **main restart writer**: serializes `ELBUF_TAB`, `NODES`, `IPARG`, connectivity, element-group state to binary file
- `rdresa.F`, `rdresb.F` — restart readers (called from `LECTUR`/`LECSTAT`)
- `rdcomm.F` — communicates restart data across MPI domains
- `read_*.F`, `read_*.F90` — type-specific restart readers (materials, EOS, failure, ALE, joints, sensors, thermal, BCS, etc.)

### `sta/` — Stats output (`.sta` file)
- `genstat.F` — writes per-cycle stats: time, DT, energy, mass errors
- `stat_brick_mp.F`, `stat_beam_mp.F`, `stat_beam_spmd.F` — element type specific stats
- `spmd_state_*.F` — MPI gather for stats across domains

### `sty/` — STY (style) output
- `genoutp.F` — main STY output writer
- `outp_*.F` — per-variable STY output routines (nodal velocity, stress, etc.)
- `c_tf_ne.F` — C TF output helpers

### `dynain/` — Dynain restart output
- Writes a Radioss input deck (`_dynain.rad`) containing the deformed mesh state

### `cluster/` — Cluster/HPC output
- Output helpers for distributed cluster environments

### `message/` — User messages
- Warning/error/info message formatting and output

### `qaprint/`, `report/` — QA and report output
- QA validation print, formatted report generators

### `outfile/` — Output file management
- `check_nan_acc.F` — checks for NaN in acceleration (debug/diagnostic)

### `tools/` — Output utilities shared between formats

## Key access pattern
All output routines access element state via `ELBUF_TAB(NG)`:
```fortran
! Typical pattern in animx.F / thsol.F:
DO NG = 1, NGROUP
  GBUF => ELBUF_TAB(NG)%GBUF
  DO IE = 1, ELBUF_TAB(NG)%NEL
    SIG = GBUF%SIG(IE)     ! mean stress for element IE in group NG
  END DO
END DO
```

## Dependencies
- Called by: `RESOL` → `SORTIE_MAIN` (Step 13)
- Uses: `ELBUF_TAB` (element state), `NODES` (nodal arrays), `IPARG`/`NGROUP`
- MPI gather for animation/H3D: `engine/source/mpi/anim/`
