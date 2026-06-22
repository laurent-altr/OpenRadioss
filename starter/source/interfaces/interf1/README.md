# Interface Setup Entry Point (`starter/source/interfaces/interf1/`)

Top-level contact geometry setup orchestrator: called after all interface cards are read to build the full contact data structures.

## Key Files

| File | Role |
|------|------|
| `lecins.F` | Read interface data block from legacy restart |
| `lecint.F` | Top-level interface read/initialise driver |
| `inintsub.F` | Sub-interface (local partition) initialisation |
| `inpoint.F` | General point-in-surface test |
| `inslin.F` | Slave node linear ordering for contact |

## Description

`lecint.F` is the main entry point called from the starter's model-setup loop. It iterates over all defined interfaces, calls `inint3.F` or `inint2.F` for each type, applies /DEF_INTER defaults, and writes the final contact data to the restart file. It coordinates with SPMD domain decomposition to assign contact pairs to the correct MPI domain.

## Related Documentation

- `starter/source/interfaces/inter3d1/README.md` — 3D contact geometry initialisation
- `starter/source/interfaces/inter2d1/README.md` — 2D contact geometry initialisation
- `starter/source/interfaces/reader/README.md` — interface card reader
