# engine/source/engine/

## Purpose
Engine binary entry point and main time-integration loop. This is the top-level
orchestration of the entire Engine run: startup, restart file read, and the
per-cycle explicit integration loop.

## Key files

| File | Role |
|------|------|
| `radioss.F` | Program entry point (~40 lines): `PROGRAM ENGINE` → `CALL RADIOSS0` |
| `radioss0.F` | Thin wrapper: calls `RADIOSS2` |
| `radioss2.F` | Full startup: initializes MPI/OpenMP, reads engine input + restart files via `LECINP`/`LECSTAT`/`LECTUR`/`RDRESA`, then calls `RESOL` |
| `resol.F` | **THE main time loop** (9729 lines): one pass = one time cycle; see `doc/ENGINE_TIME_LOOP_documentation.md` for the complete call tree |
| `resol_init.F` | Per-cycle initialization called inside `!$OMP PARALLEL` at cycle start (`RESOL_INIT`) |
| `resol_head.F` | Prints time-step header to stdout/`.sta` file each cycle (`RESOL_HEAD`) |
| `resol_alloc.F90` | Dynamic allocation helpers for `RESOL` arrays (`resol_alloc_phase*`) |
| `radioss_title.F` | Prints the OpenRadioss version/title banner |

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `node_spliting/` | Node splitting for crack propagation: `DETACH_NODE` (detaches a node at element failure), `GHOST_SHELLS` (creates phantom shell elements), `UPDATE_PON` (updates Parith/ON data after split) |

## Startup sequence (simplified)
```
radioss.F → RADIOSS0 → RADIOSS2
  LECINP / LECSTAT / LECTUR    ! read engine keywords + restart files
  RDRESA                       ! read restart state (ELBUF_TAB, stresses)
  INIPAR(…,3,…)                ! post-read parameter setup
  RESOL(TIMERS, ELEMENT, NODES, …)   ! TIME LOOP
```

## Dependencies
- See `doc/ENGINE_TIME_LOOP_documentation.md` for the detailed per-cycle breakdown
- `engine/source/input/` provides all `FRE*` and `LEC*` routines called from `RADIOSS2`
