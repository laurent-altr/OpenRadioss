# Starter Entry Point

This directory contains the main entry point and top-level control of the starter binary.

## Key Files

| File | Role |
|------|------|
| `starter.F` | **Main program** — argument parsing, MPI init, calls `LECTUR`, writes restart |
| `starter0.F` | Starter initialisation (file opening, banner, memory setup) |
| `lectur.F` | **Top-level keyword reader** — reads the input deck keyword by keyword |
| `contrl.F` | Control logic connecting parser to model assembly |
| `freform.F` | Free-format keyword detection and dispatcher |
| `execargcheck.F` | Parse and validate command-line arguments |
| `check_dynain.F` | Validate DYNAIN file format for chained simulations |
| `check_qeph_stra.F` | Validate QEPH strain output compatibility |
| `radioss_title.F` | Print OpenRadioss title banner |
| `ascii_encoding_mu_letter.F90` | ASCII encoding for the μ (micro) symbol in unit output |

## Execution Flow

```
main (starter.F)
  │
  ├── Parse arguments (execargcheck.F)
  ├── MPI_Init
  ├── starter0.F      — open files, print banner, allocate memory
  ├── LECTUR (lectur.F)
  │     — loop over keywords in the input deck:
  │       ├── /NODE, /ELEMENT, ...     → elements/
  │       ├── /MAT, /FAIL, ...         → materials/
  │       ├── /PROP, ...               → properties/
  │       ├── /INTER, ...              → interfaces/
  │       ├── /BCS, /EBCS, ...         → boundary_conditions/
  │       ├── /GRNOD, /GRSH, ...       → groups/
  │       ├── /RBODY, /RWALL, ...      → constraints/
  │       ├── /CLOAD, /PLOAD, ...      → loads/
  │       ├── /FUNCT, /SENSOR, ...     → tools/
  │       ├── /INIVEL, /INISTA, ...    → initial_conditions/
  │       └── /RUN, /PRINT, /ANIM, ... → general_controls/
  ├── contrl.F        — model assembly, validation
  ├── spmd/           — domain decomposition
  ├── output/         — write model summary, initial animation
  └── restart/        — write _0001.rad binary restart
```

## Command-Line Arguments

```
starter_linux64_gf  -i <input_file>_0000.rad  [-np <nprocs>]  [-userlib <lib.so>]
```

Key arguments:
- `-i` / `-input` — input keyword file (or HM binary with `devtools/hm_reader`)
- `-np` — number of MPI processes (must match engine invocation)
- `-nthread` — number of OpenMP threads
- `-userlib` — path to user subroutine shared library

## Related Documentation

- `starter/source/README.md` — overall starter architecture and pipeline
- `engine/source/engine/README.md` — engine entry point (analogous structure)
