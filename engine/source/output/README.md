# Output Subsystem

This subsystem writes all result data produced by the engine. It supports multiple output formats and is driven by output frequency parameters set in the input deck.

## Directory Structure

```
output/
├── anim/          — Legacy ASCII/binary animation files (A-file format)
├── cluster/       — Cluster (multi-processor) output coordination
├── dynain/        — DYNAIN restart output (deformed geometry at end of run)
├── h3d/           — H3D binary result format (primary output format)
├── message/       — Message / log file writing
├── outfile/       — Main `.out` text output file
├── qaprint/       — QA-check print output (/PRINT, /QPRINT keywords)
├── report/        — Summary report generation
├── restart/       — Engine restart file writing (for run continuation)
├── sta/           — STA (status) file output (progress information)
├── sty/           — STY (state) file — model state snapshot
├── th/            — Time History output (TH-file format)
├── tools/         — Shared output utilities
├── sortie_main.F  — Top-level output dispatch (called from time loop)
├── sortie_error.F — Error-output dispatch
├── ecrit.F        — Low-level write primitives
├── ini_outmax.F   — Initialise output max-value tracking
├── upd_outmax.F   — Update tracked max values
└── outmaxsubr.F   — Max-value output routines
```

## Output Formats

### H3D (`h3d/`) — Primary format
H3D is Altair's binary result format, readable by HyperView and other Altair tools. It contains:
- Nodal results: displacement, velocity, acceleration
- Element results: stress, strain, plastic strain, energy
- Section forces, contact forces, rigid body data

The H3D writer is split into:
- `h3d_build_cpp/` — C++ wrappers that call the H3D library (`c_h3d_*.cpp`)
- `h3d_build_fortran/` — Fortran-side data extraction and call routing
- `h3d_results/` — Per-result-type writers (stress, strain, velocity, …)
- `input_list/` — Management of the output result list
- `spmd/` — MPI-parallel H3D coordination

### Time History (`th/`) — TH-file format
Time history records requested variables at every output frequency for selected entities (nodes, elements, groups, rigid bodies, interfaces). Key files:

| File | Role |
|------|------|
| `init_th.F`, `init_th0.F` | TH output initialisation |
| `init_th_group.F` | Group-based TH initialisation |
| `hist1.F`, `hist2.F`, `hist13.F` | History record writing (nodal, element, contact) |
| `bcs1th.F`, `bcs1th_imp.F` | BCS output in TH |
| `grelem_sav.F` | Element group result saving |
| `init_reac_nod.F` | Reaction force initialisation |

### Animation (`anim/`) — Legacy A-file format
Older Altair format (pre-H3D). Still supported for backward compatibility. Files are generated in `anim/generate/` and read in `anim/reader/`.

### Restart (`restart/`)
Writes the engine restart file so a simulation can be resumed. Contains the full solution state (positions, velocities, stresses, history variables). Key files: `rdcomm.F`, `rdresa.F`, `rdresb.F`, `fillxdp.F`.

### DYNAIN (`dynain/`)
Writes a DYNAIN file at end of run — a re-startable deformed geometry file used as the initial state for a subsequent simulation (e.g. springback after forming).

### STA (`sta/`)
The `.sta` file reports simulation progress (time, dt, energy, number of elements) at regular intervals. Used for monitoring long runs.

### QAPRINT (`qaprint/`)
Writes QA-check tables to the `.out` file for selected keywords. Activated by `/QAPRINT` and printed for each `/PRINT` keyword.

## Output Dispatch (`sortie_main.F`)

The time loop in `engine/resol.F` calls `SORTIE_MAIN` at each output interval. This routine decides which writers to call based on the current time and the requested output frequencies:

```
SORTIE_MAIN
  ├── H3D writer       (if T >= T_h3d_next)
  ├── TH writer        (if T >= T_th_next)
  ├── Anim writer      (if T >= T_anim_next)
  ├── STA update       (always, low cost)
  └── Restart writer   (if T >= T_restart_next)
```

## Max-Value Tracking

`ini_outmax.F`, `upd_outmax.F`, and `outmaxsubr.F` maintain running maxima of selected element/nodal results (e.g. peak plastic strain, peak stress). These are reported in the `.out` summary at the end of the run.

## Related Documentation

- `engine/source/README.md` — time loop that drives output
- `engine/source/mpi/README.md` — parallel output coordination
