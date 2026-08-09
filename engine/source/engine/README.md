# Engine Entry Point

This directory contains the main entry point of the engine binary and the top-level time integration loop.

## Key Files

| File | Role |
|------|------|
| `radioss.F` | **Main program** — argument parsing, MPI/OpenMP init, calls `RESOL` |
| `radioss0.F` | Engine initialisation (memory, file opening, banner) |
| `radioss2.F` | Post-run cleanup and finalisation |
| `radioss_title.F` | Print the OpenRadioss title banner to output |
| `resol.F` | **Top-level time loop** — orchestrates all subsystems at each step |
| `resol_init.F` | Time loop initialisation (reads restart, sets up subsystems) |
| `resol_alloc.F90` | Allocate global working arrays for the time loop |
| `resol_head.F` | Print time-loop header to `.out` file |
| `execargcheck.F` | Parse and validate command-line arguments |
| `lf_convert_c.c` | C utility for large-file (>2GB) I/O support |
| `openmp_stub.F90` | OpenMP stub for single-threaded builds (no-op stubs for OpenMP intrinsics) |

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `node_spliting/` | Node splitting for adaptive element refinement (used by remesh) |

## Execution Flow

```
main (radioss.F)
  │
  ├── Parse arguments (execargcheck.F)
  ├── MPI_Init
  ├── radioss0.F  — open files, print banner, allocate globals
  ├── RESOL (resol.F)
  │     ├── resol_init.F   — read restart, initialise all subsystems
  │     └── time loop:
  │           ├── time_step/   — compute dt
  │           ├── assembly/    — gather kinematics
  │           ├── elements/    — compute internal forces
  │           ├── interfaces/  — compute contact forces
  │           ├── loads/       — apply external forces
  │           ├── assembly/    — scatter forces, update v and x
  │           ├── constraints/ — enforce rigid bodies
  │           └── output/      — write results (if output step)
  └── radioss2.F — print energy balance, close files, MPI_Finalize
```

## OpenMP Stub (`openmp_stub.F90`)

When OpenMP is not available at compile time, this stub provides no-op implementations of `OMP_GET_THREAD_NUM`, `OMP_GET_NUM_THREADS`, etc. This allows the same source to compile in both threaded and serial modes.

## Large-File Support (`lf_convert_c.c`)

On 32-bit systems and some older Fortran compilers, file offsets are limited to 2 GB. `lf_convert_c.c` provides C-level wrappers using `off_t` / `fseeko` for files larger than this limit.

## Related Documentation

- `engine/source/README.md` — overall engine architecture and subsystem map
